{-# LANGUAGE PackageImports #-}
import System.Environment
import System.Random
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
import Data.Text (Text)
import qualified Data.UUID as UUID
import qualified Data.UUID.V1 as UUID
import qualified Data.Text as T
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
	messageTo = parseJID (tel <> fromString "@sms"),
	messagePayloads = [Element (fromString "{jabber:client}body") [] [NodeContent $ ContentText txt]]
}

tcKey tel key = T.unpack tel <> "\0" <> key
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

code str status =
	hasAttributeText (fromString "{http://jabber.org/protocol/muc#user}code") (== (fromString str)) status
	<>
	hasAttributeText (fromString "code") (== (fromString str)) status

componentMessage db toVitelity MessageGroupChat mid existingRoom bareFrom resourceFrom tel body = do
	if fmap bareTxt existingRoom == Just bareFrom && (
	   existingRoom /= parseJID (bareFrom <> fromString "/" <> fromMaybe mempty resourceFrom) ||
	   not (fromString "CHEOGRAM%" `T.isPrefixOf` mid)) then
		writeStanzaChan toVitelity $ mkSMS tel txt
	else
		return () -- TODO: Error?
	where
	txt = mconcat [fromString "(", fromMaybe (fromString "nonick") resourceFrom, fromString ") ", body]
componentMessage _ toVitelity _ _ existingRoom bareFrom resourceFrom tel body =
	writeStanzaChan toVitelity $ mkSMS tel txt
	where
	txt = mconcat [fromString "(", fromNick, fromString " whispers) ", body]
	fromNick
		| fmap bareTxt existingRoom == Just bareFrom = fromMaybe (fromString "nonick") resourceFrom
		| otherwise = bareFrom

componentStanza db toVitelity _ (ReceivedMessage (m@Message { messageTo = Just to, messageFrom = Just from}))
	| Just tel <- strNode <$> jidNode to,
	  Just invite <- getMediatedInvitation m <|> getDirectInvitation m = do
		existingRoom <- tcGetJID db tel "joined"
		existingInvite <- tcGetJID db tel "invited"
		let txt = mconcat [
				fromString "* ",
				bareTxt (inviteFrom invite), -- TODO: or MUC nick
				fromString " has invited you to a group",
				maybe mempty (\t -> fromString ", saying \"" <> t <> fromString "\"") (inviteText invite),
				fromString "\nYou can switch to this group by sending /join"
			]
		when (existingRoom /= Just (inviteMUC invite) && existingInvite /= Just (inviteMUC invite)) $ do
			tcPutJID db tel "invited" (inviteMUC invite)
			writeStanzaChan toVitelity $ mkSMS tel txt
componentStanza db _ toComponent (ReceivedMessage (m@Message { messageTo = Just to, messageFrom = Just from}))
	| Just tel <- strNode <$> jidNode to,
	  [x] <- isNamed (fromString "{http://jabber.org/protocol/muc#user}x") =<< messagePayloads m,
	  [status] <- isNamed (fromString "{http://jabber.org/protocol/muc#user}status") =<< elementChildren x,
	  (_:_) <- code "104" status =
		queryDisco toComponent from to
componentStanza db toVitelity _ (ReceivedMessage (m@Message { messageTo = Just to, messageFrom = Just from}))
	| Just tel <- strNode <$> jidNode to,
	  Just body <- getBody "jabber:component:accept" m = do
		existingRoom <- tcGetJID db tel "joined"
		componentMessage db toVitelity (messageType m) (fromMaybe mempty $ messageID m) existingRoom (bareTxt from) resourceFrom tel body
	where
	resourceFrom = strResource <$> jidResource from
componentStanza db toVitelity toComponent (ReceivedPresence p@(Presence { presenceType = PresenceAvailable, presenceFrom = Just from, presenceTo = Just to }))
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
componentStanza db toVitelity _ (ReceivedPresence p@(Presence { presenceType = PresenceUnavailable, presenceFrom = Just from, presenceTo = Just to }))
	| Just tel <- strNode <$> jidNode to,
	  [] <- code "303" =<< isNamed (fromString "{http://jabber.org/protocol/muc#user}status") =<< elementChildren
	        =<< isNamed (fromString "{http://jabber.org/protocol/muc#user}x") =<< presencePayloads p = do
		existingRoom <- tcGetJID db tel "joined"
		when (existingRoom == Just from) $ do
			True <- TC.runTCM $ TC.out db $ tcKey tel "joined"
			writeStanzaChan toVitelity $ mkSMS tel (fromString "* You have left " <> bareTxt from)
componentStanza db _ toComponent (ReceivedPresence p@(Presence { presenceType = PresenceSubscribe, presenceFrom = Just from, presenceTo = Just to@JID { jidNode = Nothing } })) = do
	writeStanzaChan toComponent $ (emptyPresence PresenceSubscribed) {
		presenceTo = Just from,
		presenceFrom = Just to
	}
	writeStanzaChan toComponent $ (emptyPresence PresenceSubscribe) {
		presenceTo = Just from,
		presenceFrom = Just to
	}
componentStanza db _ toComponent (ReceivedPresence p@(Presence { presenceType = PresenceAvailable, presenceFrom = Just from, presenceTo = Just to@JID { jidNode = Nothing } })) = do
	writeStanzaChan toComponent $ (emptyPresence PresenceAvailable) {
		presenceTo = Just from,
		presenceFrom = Just to
	}
componentStanza _ _ toComponent (ReceivedIQ (IQ { iqType = IQGet, iqFrom = Just from, iqTo = Just to, iqID = id, iqPayload = Just p }))
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
componentStanza _ _ toComponent (ReceivedIQ (IQ { iqType = IQGet, iqFrom = Just from, iqTo = Just to, iqID = id, iqPayload = Just p }))
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
componentStanza _ _ toComponent (ReceivedIQ (IQ { iqType = IQGet, iqFrom = Just from, iqTo = Just (to@JID {jidNode = Nothing}), iqID = id, iqPayload = Just p }))
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
componentStanza db _ toComponent (ReceivedIQ (IQ { iqType = IQResult, iqFrom = Just from, iqPayload = Just p }))
	| [query] <- isNamed (fromString "{http://jabber.org/protocol/disco#info}query") p = do
		let vars = mapMaybe (attributeText (fromString "var")) $
			isNamed (fromString "{http://jabber.org/protocol/disco#info}feature") =<< elementChildren query
		let muc_membersonly = fromEnum $ fromString "muc_membersonly" `elem` vars
		True <- TC.runTCM $ TC.put db (T.unpack (formatJID from) <> "\0muc_membersonly") muc_membersonly
		return ()
componentStanza _ _ toComponent (ReceivedIQ (IQ { iqType = typ, iqFrom = Just from, iqTo = to, iqID = id }))
	| typ `elem` [IQGet, IQSet] =
		writeStanzaChan toComponent $ (emptyIQ IQError) {
			iqTo = Just from,
			iqFrom = to,
			iqID = id,
			iqPayload = Just $ Element (fromString "{jabber:component:accept}error")
				[(fromString "{jabber:component:accept}type", [ContentText $ fromString "cancel"])]
				[NodeElement $ Element (fromString "{urn:ietf:params:xml:ns:xmpp-stanzas}feature-not-implemented") [] []]
		}
componentStanza _ _ _ _ = return ()

component db toVitelity toComponent = do
	forkXMPP $ forever $ flip catchError (liftIO . print) $ do
		stanza <- liftIO $ atomically $ readTChan toComponent
		putStanza $ stanza

	--forever $ getStanza >>= liftIO . componentStanza db toVitelity
	forever $ flip catchError (liftIO . print) $ do
		s <- getStanza
		liftIO $ componentStanza db toVitelity toComponent s

data Command = Join JID | JoinInvited | Send Text | Leave | InviteCmd JID | SetNick Text
	deriving (Show, Eq)

parseCommand txt nick
	| Just jid <- T.stripPrefix (fromString "/invite ") txt =
		InviteCmd <$> parseJID jid
	| Just room <- T.stripPrefix (fromString "/join ") txt =
		Join <$> (parseJID (room <> fromString "/" <> nick) <|> parseJID room)
	| Just nick <- T.stripPrefix (fromString "/nick ") txt = Just $ SetNick nick
	| txt == fromString "/join" = Just JoinInvited
	| txt == fromString "/leave" = Just Leave
	| txt == fromString "/part" = Just Leave
	| otherwise = Just $ Send txt

getMessage (ReceivedMessage m) = Just m
getMessage _ = Nothing

leaveRoom db toComponent componentHost tel reason = do
	existingRoom <- tcGetJID db tel "joined"
	forM_ existingRoom $ \leaveRoom -> do
		writeStanzaChan toComponent $ (emptyPresence PresenceUnavailable) {
			presenceTo = Just leaveRoom,
			presenceFrom = parseJID $ tel <> fromString "@" <> fromString componentHost,
			presencePayloads = [Element (fromString "{jabber:component:accept}status") [] [NodeContent $ ContentText $ fromString reason]]
		}
		True <- TC.runTCM $ TC.out db $ tcKey tel "joined"
		return ()

joinRoom db toComponent componentHost tel room = do
	writeStanzaChan toComponent $ (emptyPresence PresenceAvailable) {
		presenceTo = Just room,
		presenceFrom = parseJID $ tel <> fromString "@" <> fromString componentHost,
		presencePayloads = [Element (fromString "{http://jabber.org/protocol/muc}x") [] [
			NodeElement $ Element (fromString "{http://jabber.org/protocol/muc}history") [(fromString "{http://jabber.org/protocol/muc}maxchars", [ContentText $ fromString "0"])] []
		]]
	}

viteltiy db toVitelity toComponent componentHost = do
	putStanza $ emptyPresence PresenceAvailable

	forkXMPP $ forever $ flip catchError (liftIO . print) $ do
		stanza <- liftIO $ atomically $ readTChan toVitelity
		putStanza $ stanza
		wait <- liftIO $ getStdRandom (randomR (400000,1500000))
		liftIO $ print ("Going to threadDelay ", wait)
		liftIO $ threadDelay wait

	forever $ flip catchError (liftIO . print) $ do
		m <- getMessage <$> getStanza
		liftIO $ case (strNode <$> (jidNode =<< messageFrom =<< m), getBody "jabber:client" =<< m) of
			(Just tel, Just txt) -> do
				nick <- maybe tel fromString <$> TC.runTCM (TC.get db $ tcKey tel "nick")
				case parseCommand txt nick of
					Just JoinInvited -> do
						invitedRoom <- tcGetJID db tel "invited"
						let toJoin = invitedRoom >>= \jid -> parseJID (bareTxt jid <> fromString "/" <> nick)
						case toJoin of
							Just room -> joinRoom db toComponent componentHost tel room
							Nothing -> writeStanzaChan toVitelity $ mkSMS tel (fromString "You have not recently been invited to a group")
					Just (Join room) -> do
						leaveRoom db toComponent componentHost tel "Joined a different room."
						joinRoom db toComponent componentHost tel room
					Just Leave -> leaveRoom db toComponent componentHost tel "Left"
					Just (InviteCmd jid) -> do
							existingRoom <- (parseJID <=< fmap bareTxt) <$> tcGetJID db tel "joined"
							forM_ existingRoom $ \room -> do
								writeStanzaChan toComponent $ (emptyMessage MessageNormal) {
									messageTo = Just room,
									messageFrom = parseJID $ tel <> fromString "@" <> fromString componentHost,
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
									messageFrom = parseJID $ tel <> fromString "@" <> fromString componentHost,
									messagePayloads = [
										Element (fromString "{jabber:x:conference}x") [
											(fromString "{jabber:x:conference}jid", [ContentText $ formatJID room])
										] [],
										Element (fromString "{jabber:component:accept}body") []
											[NodeContent $ ContentText $ mconcat [tel, fromString " has invited you to join ", formatJID room]]
									]
								}
					Just (SetNick nick) -> do
						existingRoom <- (parseJID <=< fmap bareTxt) <$> tcGetJID db tel "joined"
						forM_ existingRoom $ \room -> do
							let toJoin = parseJID (bareTxt room <> fromString "/" <> nick)
							forM_ toJoin $ joinRoom db toComponent componentHost tel

						True <- TC.runTCM (TC.put db (tcKey tel "nick") (T.unpack nick))
						return ()
					Just (Send msg) -> do
						existingRoom <- tcGetJID db tel "joined"
						case existingRoom of
							Just room -> do
								uuid <- (fmap.fmap) UUID.toString UUID.nextUUID
								writeStanzaChan toComponent $ (emptyMessage MessageGroupChat) {
									messageTo = parseJID $ bareTxt room,
									messageFrom = parseJID $ tel <> fromString "@" <> fromString componentHost,
									messageID = Just $ fromString ("CHEOGRAM%" <> fromMaybe "UUIDFAIL" uuid),
									messagePayloads = [Element (fromString "{jabber:component:accept}body") [] [NodeContent $ ContentText msg]]
								}
							Nothing -> writeStanzaChan toVitelity $ mkSMS tel (fromString "You are not joined to a room")
					Nothing -> writeStanzaChan toVitelity $ mkSMS tel (fromString "You sent an invalid message")
			_ -> return ()

openTokyoCabinet :: (TC.TCDB a) => FilePath -> IO a
openTokyoCabinet pth = TC.runTCM $ do
	db <- TC.new
	True <- TC.open db pth [TC.OREADER, TC.OWRITER, TC.OCREAT]
	return db

main = do
	[name, host, port, secret, vitelityJid, vitelityPassword] <- getArgs
	db <- openTokyoCabinet "./db.tcdb" :: IO TC.HDB
	toVitelity <- atomically newTChan
	toComponent <- atomically newTChan
	forkIO $ void $ runComponent (Server (fromString name) host (PortNumber $ fromIntegral (read port :: Int))) (fromString secret) (component db toVitelity toComponent)

	let Just vitelityParsedJid = parseJID $ fromString vitelityJid
	runClient (Server (fromString "s.ms") "s.ms" (PortNumber 5222)) vitelityParsedJid (fromMaybe mempty $ strNode <$> jidNode vitelityParsedJid) (fromString vitelityPassword) $ do
		bindJID vitelityParsedJid
		viteltiy db toVitelity toComponent name
