{-# LANGUAGE PackageImports #-}
import System.Environment
import Data.Time
import Data.Char
import System.Random
import System.Random.Shuffle (shuffleM)
import Data.String
import Network
import Network.Protocol.XMPP
import Data.List
import Data.Foldable (forM_)
import Control.Monad hiding (forM_)
import Control.Monad.IO.Class
import Data.String
import Data.XML.Types
import Control.Applicative
import Data.Monoid
import Data.Maybe
import "monads-tf" Control.Monad.Error (catchError)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.UUID as UUID
import qualified Data.UUID.V1 as UUID
import qualified Database.TokyoCabinet as TC

data StanzaRec = StanzaRec (Maybe JID) (Maybe JID) (Maybe Text) (Maybe Text) [Element] Element deriving (Show)
mkStanzaRec x = StanzaRec (stanzaTo x) (stanzaFrom x) (stanzaID x) (stanzaLang x) (stanzaPayloads x) (stanzaToElement x)
instance Stanza StanzaRec where
	stanzaTo (StanzaRec to _ _ _ _ _) = to
	stanzaFrom (StanzaRec _ from _ _ _ _) = from
	stanzaID (StanzaRec _ _ id _ _ _) = id
	stanzaLang (StanzaRec _ _ _ lang _ _) = lang
	stanzaPayloads (StanzaRec _ _ _ _ payloads _) = payloads
	stanzaToElement (StanzaRec _ _ _ _ _ element) = element

writeStanzaChan chan = atomically . writeTChan chan . mkStanzaRec

mkSMS tel txt = (emptyMessage MessageChat) {
	messageTo = telToVitelity tel,
	messagePayloads = [Element (fromString "{jabber:client}body") [] [NodeContent $ ContentText txt]]
}

tcKey tel key = fromMaybe "BADTEL" (fmap T.unpack $ normalizeTel tel) <> "\0" <> key
tcGetJID db tel key = (parseJID . fromString =<<) <$> TC.runTCM (TC.get db $ tcKey tel key)
tcPutJID db tel key jid = do
	True <- TC.runTCM (TC.put db (tcKey tel key) (T.unpack $ formatJID jid))
	return ()

getBody ns = listToMaybe . fmap (mconcat . elementText) . (isNamed (Name (fromString "body") (Just $ fromString ns) Nothing) <=< messagePayloads)

queryDisco toComponent to from = do
	uuid <- (fmap.fmap) (fromString . UUID.toString) UUID.nextUUID
	writeStanzaChan toComponent $ (emptyIQ IQGet) {
		iqTo = Just to,
		iqFrom = Just from,
		iqID = uuid,
		iqPayload = Just $ Element (fromString "{http://jabber.org/protocol/disco#info}query") [] []
	}

data Invite = Invite {
	inviteMUC :: JID,
	inviteFrom :: JID,
	inviteText :: Maybe Text,
	invitePassword :: Maybe Text
} deriving (Show)

getMediatedInvitation m = do
	from <- messageFrom m
	x <- listToMaybe $ isNamed (fromString "{http://jabber.org/protocol/muc#user}x") =<< messagePayloads m
	invite <- listToMaybe $ isNamed (fromString "{http://jabber.org/protocol/muc#user}invite") =<< elementChildren x
	inviteFrom <- parseJID =<< attributeText (fromString "from") invite
	return $ Invite {
		inviteMUC = from,
		inviteFrom = inviteFrom,
		inviteText = do
			txt <- mconcat . elementText <$> listToMaybe
				(isNamed (fromString "{http://jabber.org/protocol/muc#user}reason") =<< elementChildren invite)
			guard (not $ T.null txt)
			return txt,
		invitePassword =
			mconcat . elementText <$> listToMaybe
			(isNamed (fromString "{http://jabber.org/protocol/muc#user}password") =<< elementChildren x)
	}

getDirectInvitation m = do
	x <- listToMaybe $ isNamed (fromString "{jabber:x:conference}x") =<< messagePayloads m
	Invite <$>
		(parseJID =<< attributeText (fromString "jid") x) <*>
		messageFrom m <*>
		Just (do
			txt <- attributeText (fromString "reason") x
			guard (not $ T.null txt)
			return txt
		) <*>
		Just (attributeText (fromString "password") x)

forkXMPP :: XMPP () -> XMPP ThreadId
forkXMPP kid = do
	session <- getSession
	liftIO $ forkIO $ void $ runXMPP session kid

bareTxt (JID (Just node) domain _) = mconcat [strNode node, fromString "@", strDomain domain]
bareTxt (JID Nothing domain _) = strDomain domain

nickFor db jid existingRoom
	| fmap bareTxt existingRoom == Just bareFrom = return $ fromMaybe (fromString "nonick") resourceFrom
	| Just tel <- normalizeTel =<< strNode <$> jidNode jid = do
		mnick <- TC.runTCM (TC.get db $ tcKey tel "nick")
		case mnick of
			Just nick -> return (tel <> fromString " \"" <> fromString nick <> fromString "\"")
			Nothing -> return tel
	| otherwise = return $ bareFrom
	where
	bareFrom = bareTxt jid
	resourceFrom = strResource <$> jidResource jid

code str status =
	hasAttributeText (fromString "{http://jabber.org/protocol/muc#user}code") (== (fromString str)) status
	<>
	hasAttributeText (fromString "code") (== (fromString str)) status

componentMessage db toVitelity (m@Message { messageType = MessageError }) _ _ _ tel body = do
	let errorTxt = fmap (mconcat . elementText) $ listToMaybe $
		isNamed (fromString "{urn:ietf:params:xml:ns:xmpp-stanzas}text") =<<
		elementChildren =<< isNamed (fromString "{jabber:component:accept}error") =<< messagePayloads m
	writeStanzaChan toVitelity $ mkSMS tel $
		mconcat [
			fromString "(ERROR from ",
			maybe (fromString "unspecified") formatJID (messageFrom m),
			fromString ")",
			maybe mempty (fromString "\n"<>) errorTxt,
			maybe mempty (fromString "\n"<>) body
		]
componentMessage db toVitelity m existingRoom _ _ tel _
	| Just invite <- getMediatedInvitation m <|> getDirectInvitation m = do
		existingInvite <- tcGetJID db tel "invited"
		nick <- nickFor db (inviteFrom invite) existingRoom
		let txt = mconcat [
				fromString "* ",
				nick,
				fromString " has invited you to a group",
				maybe mempty (\t -> fromString ", saying \"" <> t <> fromString "\"") (inviteText invite),
				fromString "\nYou can switch to this group by sending /join"
			]
		when (existingRoom /= Just (inviteMUC invite) && existingInvite /= Just (inviteMUC invite)) $ do
			tcPutJID db tel "invited" (inviteMUC invite)
			writeStanzaChan toVitelity $ mkSMS tel txt
componentMessage db toVitelity (m@Message { messageType = MessageGroupChat }) existingRoom bareFrom resourceFrom tel (Just body) = do
	if fmap bareTxt existingRoom == Just bareFrom && (
	   existingRoom /= parseJID (bareFrom <> fromString "/" <> fromMaybe mempty resourceFrom) ||
	   not (fromString "CHEOGRAM%" `T.isPrefixOf` (fromMaybe mempty $ messageID m))) then
		writeStanzaChan toVitelity $ mkSMS tel txt
	else
		return () -- TODO: Error?
	where
	txt = mconcat [fromString "(", fromMaybe (fromString "nonick") resourceFrom, fromString ") ", body]
componentMessage db toVitelity (Message { messageFrom = Just from }) existingRoom _ _ tel (Just body) = do
	nick <- nickFor db from existingRoom
	let txt = mconcat [fromString "(", nick, fromString " whispers) ", body]
	writeStanzaChan toVitelity $ mkSMS tel txt
componentMessage _ _ _ _ _ _ _ _ = return ()

componentStanza db _ toComponent _ (ReceivedMessage (m@Message { messageTo = Just to, messageFrom = Just from}))
	| [x] <- isNamed (fromString "{http://jabber.org/protocol/muc#user}x") =<< messagePayloads m,
	  [status] <- isNamed (fromString "{http://jabber.org/protocol/muc#user}status") =<< elementChildren x,
	  (_:_) <- code "104" status =
		queryDisco toComponent from to
componentStanza db toVitelity toComponent componentHost (ReceivedMessage (m@Message { messageTo = Just to, messageFrom = Just from}))
	| Just tel <- strNode <$> jidNode to,
	  T.length tel == 11 && (fromString "1") `T.isPrefixOf` tel = do
		existingRoom <- tcGetJID db tel "joined"
		componentMessage db toVitelity m existingRoom (bareTxt from) resourceFrom tel $
			getBody "jabber:component:accept" m
	| Just jid <- (`telToJid` (fromString componentHost)) =<< strNode <$> jidNode to =
		writeStanzaChan toComponent $ m {
			messageFrom = Just to,
			messageTo = Just from,
			messageType = MessageError,
			messagePayloads = messagePayloads m <> [
				Element (fromString "{jabber:component:accept}error")
				[(fromString "{jabber:component:accept}type", [ContentText $ fromString "cancel"])]
				[
					NodeElement $ Element (fromString "{urn:ietf:params:xml:ns:xmpp-stanzas}gone") []
						[NodeContent $ ContentText $ formatJID jid],
					NodeElement $ Element (fromString "{urn:ietf:params:xml:ns:xmpp-stanzas}text")
						[(fromString "xml:lang", [ContentText $ fromString "en"])]
						[NodeContent $ ContentText $ fromString "JID must include country code: " <> formatJID jid]
				]
			]
		}
	| otherwise = writeStanzaChan toComponent $ m {
			messageFrom = Just to,
			messageTo = Just from,
			messageType = MessageError,
			messagePayloads = messagePayloads m <> [
				Element (fromString "{jabber:component:accept}error")
				[(fromString "{jabber:component:accept}type", [ContentText $ fromString "cancel"])]
				[NodeElement $ Element (fromString "{urn:ietf:params:xml:ns:xmpp-stanzas}item-not-found") [] []]
			]
		}
	where
	resourceFrom = strResource <$> jidResource from
componentStanza _ toVitelity _ _ (ReceivedPresence p@(Presence { presenceType = PresenceError, presenceFrom = Just from, presenceTo = Just to }))
	| Just tel <- strNode <$> jidNode to,
	  [_] <- isNamed (fromString "{http://jabber.org/protocol/muc}x") =<< presencePayloads p =
		writeStanzaChan toVitelity $ mkSMS tel (fromString "* Failed to join " <> bareTxt from)
componentStanza db toVitelity toComponent _ (ReceivedPresence p@(Presence { presenceType = PresenceAvailable, presenceFrom = Just from, presenceTo = Just to }))
	| Just tel <- strNode <$> jidNode to,
	  [x] <- isNamed (fromString "{http://jabber.org/protocol/muc#user}x") =<< presencePayloads p,
	  [status] <- isNamed (fromString "{http://jabber.org/protocol/muc#user}status") =<< elementChildren x,
	  (_:_) <- code "110" status = do
		existingInvite <- tcGetJID db tel "invited"
		when (existingInvite == parseJID bareMUC) $ do
			True <- TC.runTCM $ TC.out db $ tcKey tel "invited"
			return ()
		tcPutJID db tel "joined" from
		writeStanzaChan toVitelity $ mkSMS tel (mconcat [fromString "* You have joined ", bareMUC, fromString " as ", roomNick])
		queryDisco toComponent (fromMaybe (error "bareMUC not actually a JID") $ parseJID bareMUC) to
	where
	bareMUC = bareTxt from
	roomNick = fromMaybe mempty (strResource <$> jidResource from)
componentStanza db toVitelity _ _ (ReceivedPresence p@(Presence { presenceType = PresenceUnavailable, presenceFrom = Just from, presenceTo = Just to }))
	| Just tel <- strNode <$> jidNode to,
	  [] <- code "303" =<< isNamed (fromString "{http://jabber.org/protocol/muc#user}status") =<< elementChildren
	        =<< isNamed (fromString "{http://jabber.org/protocol/muc#user}x") =<< presencePayloads p = do
		existingRoom <- tcGetJID db tel "joined"
		when (existingRoom == Just from) $ do
			True <- TC.runTCM $ TC.out db $ tcKey tel "joined"
			writeStanzaChan toVitelity $ mkSMS tel (fromString "* You have left " <> bareTxt from)
componentStanza db _ toComponent _ (ReceivedPresence p@(Presence { presenceType = PresenceSubscribe, presenceFrom = Just from, presenceTo = Just to@JID { jidNode = Nothing } })) = do
	writeStanzaChan toComponent $ (emptyPresence PresenceSubscribed) {
		presenceTo = Just from,
		presenceFrom = Just to
	}
	writeStanzaChan toComponent $ (emptyPresence PresenceSubscribe) {
		presenceTo = Just from,
		presenceFrom = Just to
	}
componentStanza db _ toComponent _ (ReceivedPresence p@(Presence { presenceType = PresenceAvailable, presenceFrom = Just from, presenceTo = Just to@JID { jidNode = Nothing } })) = do
	writeStanzaChan toComponent $ (emptyPresence PresenceAvailable) {
		presenceTo = Just from,
		presenceFrom = Just to
	}
componentStanza _ _ toComponent _ (ReceivedIQ (IQ { iqType = IQGet, iqFrom = Just from, iqTo = Just to, iqID = id, iqPayload = Just p }))
	| Nothing <- jidNode to,
	  [_] <- isNamed (fromString "{http://jabber.org/protocol/disco#info}query") p =
		writeStanzaChan toComponent $ (emptyIQ IQResult) {
			iqTo = Just from,
			iqFrom = Just to,
			iqID = id,
			iqPayload = Just $ Element (fromString "{http://jabber.org/protocol/disco#info}query") []
				[
					NodeElement $ Element (fromString "{http://jabber.org/protocol/disco#info}identity") [
						(fromString "{http://jabber.org/protocol/disco#info}category", [ContentText $ fromString "gateway"]),
						(fromString "{http://jabber.org/protocol/disco#info}type", [ContentText $ fromString "sms"]),
						(fromString "{http://jabber.org/protocol/disco#info}name", [ContentText $ fromString "Cheogram SMS Gateway"])
					] []
				]
		}
componentStanza _ _ toComponent _ (ReceivedIQ (IQ { iqType = IQGet, iqFrom = Just from, iqTo = Just to, iqID = id, iqPayload = Just p }))
	| Just _ <- jidNode to,
	  [_] <- isNamed (fromString "{http://jabber.org/protocol/disco#info}query") p =
		writeStanzaChan toComponent $ (emptyIQ IQResult) {
			iqTo = Just from,
			iqFrom = Just to,
			iqID = id,
			iqPayload = Just $ Element (fromString "{http://jabber.org/protocol/disco#info}query") []
				[
					NodeElement $ Element (fromString "{http://jabber.org/protocol/disco#info}feature") [
						(fromString "{http://jabber.org/protocol/disco#info}var", [ContentText $ fromString "jabber:x:conference"])
					] []
				]
		}
componentStanza _ _ toComponent componentHost (ReceivedIQ (iq@IQ { iqType = IQSet, iqFrom = Just from, iqTo = Just (to@JID {jidNode = Nothing}), iqID = id, iqPayload = Just p }))
	| [query] <- isNamed (fromString "{jabber:iq:gateway}query") p,
	  [prompt] <- isNamed (fromString "{jabber:iq:gateway}prompt") =<< elementChildren query =
		case telToJid (T.filter isDigit $ mconcat $ elementText prompt) (fromString componentHost) of
			Just jid ->
				writeStanzaChan toComponent $ (emptyIQ IQResult) {
					iqTo = Just from,
					iqFrom = Just to,
					iqID = id,
					iqPayload = Just $ Element (fromString "{jabber:iq:gateway}query") []
						[NodeElement $ Element (fromString "{jabber:iq:gateway}jid") [ ] [NodeContent $ ContentText $ formatJID jid]]
				}
			Nothing ->
				writeStanzaChan toComponent $ iq {
					iqTo = Just from,
					iqFrom = Just to,
					iqType = IQError,
					iqPayload = Just $ Element (fromString "{jabber:component:accept}error")
						[(fromString "{jabber:component:accept}type", [ContentText $ fromString "modify"])]
						[
							NodeElement $ Element (fromString "{urn:ietf:params:xml:ns:xmpp-stanzas}not-acceptable") [] [],
							NodeElement $ Element (fromString "{urn:ietf:params:xml:ns:xmpp-stanzas}text")
								[(fromString "xml:lang", [ContentText $ fromString "en"])]
								[NodeContent $ ContentText $ fromString "Only US/Canada telephone numbers accepted"]
						]
				}
componentStanza _ _ toComponent _ (ReceivedIQ (IQ { iqType = IQGet, iqFrom = Just from, iqTo = Just (to@JID {jidNode = Nothing}), iqID = id, iqPayload = Just p }))
	| [_] <- isNamed (fromString "{jabber:iq:gateway}query") p =
		writeStanzaChan toComponent $ (emptyIQ IQResult) {
			iqTo = Just from,
			iqFrom = Just to,
			iqID = id,
			iqPayload = Just $ Element (fromString "{jabber:iq:gateway}query") []
				[
					NodeElement $ Element (fromString "{jabber:iq:gateway}desc") [ ] [NodeContent $ ContentText $ fromString "Please enter your contact's phone number"],
					NodeElement $ Element (fromString "{jabber:iq:gateway}prompt") [ ] [NodeContent $ ContentText $ fromString "Phone Number"]
				]
		}
componentStanza db _ toComponent componentHost (ReceivedIQ (IQ { iqType = IQError, iqFrom = Just from, iqTo = Just to }))
	| (strNode <$> jidNode to) == Just (fromString "create"),
	  Just resource <- strResource <$> jidResource to =
		case T.splitOn (fromString "|") resource of
			(tel:_) -> do
				nick <- maybe tel fromString <$> TC.runTCM (TC.get db $ tcKey tel "nick")
				let Just room = parseJID $ bareTxt from <> fromString "/" <> nick
				leaveRoom db toComponent componentHost tel "Joined a different room."
				joinRoom db toComponent componentHost tel room
			_ -> return () -- Invalid packet, ignore
componentStanza db _ toComponent componentHost (ReceivedIQ (IQ { iqType = IQResult, iqFrom = Just from, iqTo = Just to }))
	| (strNode <$> jidNode to) == Just (fromString "create"),
	  Just resource <- strResource <$> jidResource to =
		case map T.unpack $ T.splitOn (fromString "|") resource of
			(tel:name:[]) -> void $ createRoom toComponent componentHost [T.unpack $ strDomain $ jidDomain from] tel (name <> "_" <> tel)
			(tel:name:servers) -> void $ createRoom toComponent componentHost servers tel name
			_ -> return () -- Invalid packet, ignore
componentStanza db _ toComponent _ (ReceivedIQ (IQ { iqType = IQResult, iqFrom = Just from, iqPayload = Just p }))
	| [query] <- isNamed (fromString "{http://jabber.org/protocol/disco#info}query") p = do
		let vars = mapMaybe (attributeText (fromString "var")) $
			isNamed (fromString "{http://jabber.org/protocol/disco#info}feature") =<< elementChildren query
		let muc_membersonly = fromEnum $ fromString "muc_membersonly" `elem` vars
		True <- TC.runTCM $ TC.put db (T.unpack (formatJID from) <> "\0muc_membersonly") muc_membersonly
		return ()
componentStanza _ _ toComponent _ (ReceivedIQ (iq@IQ { iqType = typ }))
	| typ `elem` [IQGet, IQSet] =
		writeStanzaChan toComponent $ iq {
			iqTo = iqFrom iq,
			iqFrom = iqTo iq,
			iqType = IQError,
			iqPayload = Just $ Element (fromString "{jabber:component:accept}error")
				[(fromString "{jabber:component:accept}type", [ContentText $ fromString "cancel"])]
				[NodeElement $ Element (fromString "{urn:ietf:params:xml:ns:xmpp-stanzas}feature-not-implemented") [] []]
		}
componentStanza _ _ _ _ _ = return ()

component db toVitelity toComponent componentHost = do
	forkXMPP $ forever $ flip catchError (liftIO . print) $ do
		stanza <- liftIO $ atomically $ readTChan toComponent
		putStanza $ stanza

	--forever $ getStanza >>= liftIO . componentStanza db toVitelity
	forever $ flip catchError (liftIO . print) $ do
		s <- getStanza
		liftIO $ componentStanza db toVitelity toComponent componentHost s

telToVitelity tel
	| not $ all isDigit $ T.unpack tel = Nothing
	| T.length tel == 10 = parseJID (tel <> fromString "@sms")
	| T.length tel == 11, Just tel' <- T.stripPrefix (fromString "1") tel = parseJID (tel' <> fromString "@sms")
	| otherwise = Nothing

normalizeTel tel
	| not $ all isDigit $ T.unpack tel = Nothing
	| T.length tel == 10 = Just $ T.cons '1' tel
	| T.length tel == 11, (fromString "1") `T.isPrefixOf` tel = Just tel
	| otherwise = Nothing

telToJid tel host = parseJID =<< (<> fromString "@" <> host) <$> normalizeTel tel

parseJIDrequireNode txt
	| Just _ <- jidNode =<< jid = jid
	| otherwise = Nothing
	where
	jid = parseJID txt

data Command = Help | Create Text | Join JID | JoinInvited | Send Text | Leave | InviteCmd JID | SetNick Text | Whisper JID Text
	deriving (Show, Eq)

parseCommand txt room nick componentHost
	| Just jid <- T.stripPrefix (fromString "/invite ") txt =
		InviteCmd <$> (
			parseJIDrequireNode jid <|>
			telToJid jid (fromString componentHost)
		)
	| Just room <- T.stripPrefix (fromString "/join ") txt =
		Join <$> (parseJID (room <> fromString "/" <> nick) <|> parseJID room)
	| Just t <- T.stripPrefix (fromString "/create ") txt = Just $ Create t
	| Just nick <- T.stripPrefix (fromString "/nick ") txt = Just $ SetNick nick
	| Just input <- T.stripPrefix (fromString "/msg ") txt =
		let (to, msg) = T.breakOn (fromString " ") input in
		Whisper <$> (
			parseJIDrequireNode to <|>
			telToJid to (fromString componentHost) <|>
			(parseJID =<< fmap (\r -> bareTxt r <> fromString "/" <> to) room)
		) <*> pure msg
	| txt == fromString "/join" = Just JoinInvited
	| txt == fromString "/leave" = Just Leave
	| txt == fromString "/part" = Just Leave
	| txt == fromString "/help" = Just Help
	| otherwise = Just $ Send txt

getMessage (ReceivedMessage m) = Just m
getMessage _ = Nothing

sendToRoom toComponent componentHost tel room msg = do
	uuid <- (fmap.fmap) UUID.toString UUID.nextUUID
	writeStanzaChan toComponent $ (emptyMessage MessageGroupChat) {
		messageTo = parseJID $ bareTxt room,
		messageFrom = telToJid tel (fromString componentHost),
		messageID = Just $ fromString ("CHEOGRAM%" <> fromMaybe "UUIDFAIL" uuid),
		messagePayloads = [Element (fromString "{jabber:component:accept}body") [] [NodeContent $ ContentText msg]]
	}

leaveRoom db toComponent componentHost tel reason = do
	existingRoom <- tcGetJID db tel "joined"
	forM_ existingRoom $ \leaveRoom -> do
		writeStanzaChan toComponent $ (emptyPresence PresenceUnavailable) {
			presenceTo = Just leaveRoom,
			presenceFrom = telToJid tel (fromString componentHost),
			presencePayloads = [Element (fromString "{jabber:component:accept}status") [] [NodeContent $ ContentText $ fromString reason]]
		}
		return ()

joinRoom db toComponent componentHost tel room = do
	writeStanzaChan toComponent $ (emptyPresence PresenceAvailable) {
		presenceTo = Just room,
		presenceFrom = telToJid tel (fromString componentHost),
		presencePayloads = [Element (fromString "{http://jabber.org/protocol/muc}x") [] [
			NodeElement $ Element (fromString "{http://jabber.org/protocol/muc}history") [(fromString "{http://jabber.org/protocol/muc}maxchars", [ContentText $ fromString "0"])] []
		]]
	}

createRoom :: TChan StanzaRec -> String -> [String] -> String -> String -> IO Bool
createRoom toComponent componentHost (server:otherServers) tel name =
	-- First we check if this room exists on the server already
	case to of
		Just t -> queryDisco toComponent t jid >> return True
		Nothing -> return False
	where
	-- TODO: to
	to = parseJID $ fromString $ name <> "@" <> server
	Just jid = parseJID $ fromString $ "create@" <> componentHost <> "/" <> intercalate "|" (tel:name:otherServers)

processSMS db toVitelity toComponent componentHost conferenceServers tel txt = do
	nick <- maybe tel fromString <$> TC.runTCM (TC.get db $ tcKey tel "nick")
	existingRoom <- (parseJID <=< fmap bareTxt) <$> tcGetJID db tel "joined"
	case parseCommand txt existingRoom nick componentHost of
		Just JoinInvited -> do
			invitedRoom <- tcGetJID db tel "invited"
			let toJoin = invitedRoom >>= \jid -> parseJID (bareTxt jid <> fromString "/" <> nick)
			case toJoin of
				Just room -> do
					leaveRoom db toComponent componentHost tel "Joined a different room."
					joinRoom db toComponent componentHost tel room
				Nothing -> writeStanzaChan toVitelity $ mkSMS tel (fromString "You have not recently been invited to a group")
		Just (Create name) -> do
			servers <- shuffleM conferenceServers
			validRoom <- createRoom toComponent componentHost servers (T.unpack tel) (T.unpack name)
			when (not validRoom) $
				writeStanzaChan toVitelity $ mkSMS tel (fromString "Invalid room name")
		Just (Join room) -> do
			leaveRoom db toComponent componentHost tel "Joined a different room."
			joinRoom db toComponent componentHost tel room
		Just Leave -> leaveRoom db toComponent componentHost tel "Typed /leave"
		Just (InviteCmd jid)
			| Just room <- existingRoom -> do
				writeStanzaChan toComponent $ (emptyMessage MessageNormal) {
					messageTo = Just room,
					messageFrom = telToJid tel (fromString componentHost),
					messagePayloads = [
						Element (fromString "{http://jabber.org/protocol/muc#user}x") [] [
							NodeElement $ Element (fromString "{http://jabber.org/protocol/muc#user}invite") [
								(fromString "{http://jabber.org/protocol/muc#user}to", [ContentText $ formatJID jid])
							] []
						]
					]
				}

				writeStanzaChan toComponent $ (emptyMessage MessageNormal) {
					messageTo = Just jid,
					messageFrom = telToJid tel (fromString componentHost),
					messagePayloads = [
						Element (fromString "{jabber:x:conference}x") [
							(fromString "{jabber:x:conference}jid", [ContentText $ formatJID room])
						] [],
						Element (fromString "{jabber:component:accept}body") []
							[NodeContent $ ContentText $ mconcat [tel, fromString " has invited you to join ", formatJID room]]
					]
				}
			| otherwise -> writeStanzaChan toVitelity $ mkSMS tel (fromString "You are not joined to a room")
		Just (SetNick nick) -> do
			forM_ existingRoom $ \room -> do
				let toJoin = parseJID (bareTxt room <> fromString "/" <> nick)
				forM_ toJoin $ joinRoom db toComponent componentHost tel

			True <- TC.runTCM (TC.put db (tcKey tel "nick") (T.unpack nick))
			return ()
		Just (Whisper to msg) -> do
			uuid <- (fmap.fmap) UUID.toString UUID.nextUUID
			writeStanzaChan toComponent $ (emptyMessage MessageChat) {
				messageTo = Just to,
				messageFrom = telToJid tel (fromString componentHost),
				messageID = Just $ fromString ("CHEOGRAM%" <> fromMaybe "UUIDFAIL" uuid),
				messagePayloads = [Element (fromString "{jabber:component:accept}body") [] [NodeContent $ ContentText msg]]
			}
		Just (Send msg)
			| (fromString "(SMSSERVER) ") `T.isPrefixOf` msg -> return () -- bogus message from vitelity, ignore
			| Just room <- existingRoom -> sendToRoom toComponent componentHost tel room msg
			| otherwise -> writeStanzaChan toVitelity $ mkSMS tel (fromString "You are not joined to a room")
		Just Help -> writeStanzaChan toVitelity $ mkSMS tel $ fromString $ mconcat [
				"/create (one-word group name) - create new group\n",
				"/nick (desired name) - set nick\n",
				"/invite (number or JID) - invite to group\n",
				"/msg (user) - whisper to group member\n",
				"/leave - leave group"
			]
		Nothing -> writeStanzaChan toVitelity $ mkSMS tel (fromString "You sent an invalid message")

viteltiy db chunks toVitelity toComponent componentHost conferenceServers = do
	putStanza $ emptyPresence PresenceAvailable

	forkXMPP $ forever $ flip catchError (liftIO . print) $ do
		wait <- liftIO $ getStdRandom (randomR (400000,1500000))
		stanza <- liftIO $ atomically $ readTChan toVitelity
		forM_ (strNode <$> (jidNode =<< stanzaTo stanza)) $ \tel -> do
			welcomed <- maybe False toEnum <$> liftIO (TC.runTCM $ TC.get db $ tcKey tel "welcomed")
			when (not welcomed) $ do
				putStanza $ mkSMS tel $ fromString "Welcome to CheoGram! You can chat with groups of your friends, right here by SMS. Send /help to learn more."
				True <- liftIO (TC.runTCM $ TC.put db (tcKey tel "welcomed") (fromEnum True))
				liftIO $ threadDelay wait

		putStanza $ stanza
		liftIO $ threadDelay wait

	forever $ flip catchError (liftIO . print) $ do
		m <- getMessage <$> getStanza
		liftIO $ case (strNode <$> (jidNode =<< messageFrom =<< m), getBody "jabber:client" =<< m) of
			(Just tel, Just txt) ->
				case parseOnly (chunkParser tel) txt of
					Left _ -> processSMS db toVitelity toComponent componentHost conferenceServers tel txt
					Right chunk -> atomically $ writeTChan chunks chunk
			_ -> return ()

data Chunk = Chunk Text Int Int Text | TimerExpire

chunkParser tel =
	Chunk tel <$>
	(string (fromString "part:") *> decimal) <*>
	(string (fromString ":of:") *> decimal) <*>
	(string (fromString ":") *> takeText)

multipartStitcher db chunks toVitelity toComponent componentHost conferenceServers =
	go mempty
	where
	go state = do
		chunk <- atomically $ readTChan chunks
		time <- getCurrentTime
		let (done, cont) = case chunk of
			Chunk tel part total txt ->
				Map.partitionWithKey (\(_,total) (_, items) -> total == Map.size items) $
				Map.insertWith (\(time, items') (_, items) ->
					(time, items' <> items)
				) (tel,total) (time, Map.singleton part txt) state
			_ -> (mempty, state)

		forM_ (Map.toList done) $ \((tel, _), (_, items)) ->
			processSMS db toVitelity toComponent componentHost conferenceServers tel $
				mconcat $ map snd $ Map.toAscList items

		let (expired, unexpired) = Map.partition (\(t, _) -> time > 60 `addUTCTime` t) cont
		forM_ (Map.keys expired) $ \(tel, total) ->
			writeStanzaChan toVitelity $ mkSMS tel $ mconcat [
				fromString "Not all parts of your message with ",
				fromString (show total),
				fromString " parts arrived. Please send again."
			]

		go unexpired

openTokyoCabinet :: (TC.TCDB a) => FilePath -> IO a
openTokyoCabinet pth = TC.runTCM $ do
	db <- TC.new
	True <- TC.open db pth [TC.OREADER, TC.OWRITER, TC.OCREAT]
	return db

main = do
	(name:host:port:secret:vitelityJid:vitelityPassword:conferences) <- getArgs
	db <- openTokyoCabinet "./db.tcdb" :: IO TC.HDB
	chunks <- atomically newTChan
	toVitelity <- atomically newTChan
	toComponent <- atomically newTChan

	forkIO $ forever $ threadDelay 1500000 >> atomically (writeTChan chunks TimerExpire)
	forkIO $ multipartStitcher db chunks toVitelity toComponent name conferences

	forkIO $ void $ runComponent (Server (fromString name) host (PortNumber $ fromIntegral (read port :: Int))) (fromString secret) (component db toVitelity toComponent name)

	let Just vitelityParsedJid = parseJID $ fromString vitelityJid
	runClient (Server (fromString "s.ms") "s.ms" (PortNumber 5222)) vitelityParsedJid (fromMaybe mempty $ strNode <$> jidNode vitelityParsedJid) (fromString vitelityPassword) $ do
		bindJID vitelityParsedJid
		viteltiy db chunks toVitelity toComponent name conferences
