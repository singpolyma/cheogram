{-# LANGUAGE PackageImports #-}
import Prelude (show, read)
import BasicPrelude hiding (show, read, forM_, mapM_, getArgs)
import Data.Char
import Control.Concurrent
import Control.Concurrent.STM
import Data.Foldable (forM_, mapM_)
import System.Environment (getArgs)
import Control.Error (readZ)
import Data.Time (addUTCTime, getCurrentTime)
import Network (PortID(PortNumber))
import System.Random (Random(randomR), getStdRandom)
import System.Random.Shuffle (shuffleM)

import "monads-tf" Control.Monad.Error (catchError) -- ick
import Data.Attoparsec.Text (takeText, string, parseOnly, decimal)
import Data.XML.Types (Element(..), Node(NodeContent, NodeElement), Name(Name), Content(ContentText), isNamed, hasAttributeText, elementText, elementChildren, attributeText)
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.UUID as UUID ( toString )
import qualified Data.UUID.V1 as UUID ( nextUUID )
import qualified Database.TokyoCabinet as TC
import Network.Protocol.XMPP -- should import qualified

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

tcKey tel key = maybe "BADTEL" T.unpack (normalizeTel tel) <> "\0" <> key
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

fillFormField var value form = form {
		elementNodes = map (\node ->
			case node of
				NodeElement el
					| elementName el == fromString "{jabber:x:data}field" &&
					  (attributeText (fromString "{jabber:x:data}var") el == Just var ||
					  attributeText (fromString "var") el == Just var) ->
						NodeElement $ el { elementNodes = [
							NodeElement $ Element (fromString "{jabber:x:data}value") []
								[NodeContent $ ContentText value]
						]}
				x -> x
		) (elementNodes form)
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
	return Invite {
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
	| otherwise = return bareFrom
	where
	bareFrom = bareTxt jid
	resourceFrom = strResource <$> jidResource jid

code str status =
	hasAttributeText (fromString "{http://jabber.org/protocol/muc#user}code") (== fromString str) status
	<>
	hasAttributeText (fromString "code") (== fromString str) status

componentMessage _ toVitelity (m@Message { messageType = MessageError }) _ _ _ tel body = do
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
		forM_ (invitePassword invite) $ \password -> do
			True <- TC.runTCM $ TC.put db (tcKey tel (T.unpack (formatJID $ inviteMUC invite) <> "\0muc_roomsecret")) (T.unpack password)
			return ()
		existingInvite <- tcGetJID db tel "invited"
		nick <- nickFor db (inviteFrom invite) existingRoom
		let txt = mconcat [
				fromString "* ",
				nick,
				fromString " has invited you to a group",
				maybe mempty (\t -> fromString ", saying \"" <> t <> fromString "\"") (inviteText invite),
				fromString "\nYou can switch to this group by replying with /join"
			]
		when (existingRoom /= Just (inviteMUC invite) && existingInvite /= Just (inviteMUC invite)) $ do
			tcPutJID db tel "invited" (inviteMUC invite)
			writeStanzaChan toVitelity $ mkSMS tel txt
componentMessage _ toVitelity (m@Message { messageType = MessageGroupChat }) existingRoom bareFrom resourceFrom tel (Just body) =
	if fmap bareTxt existingRoom == Just bareFrom && (
	   existingRoom /= parseJID (bareFrom <> fromString "/" <> fromMaybe mempty resourceFrom) ||
	   not (fromString "CHEOGRAM%" `T.isPrefixOf` fromMaybe mempty (messageID m))) then
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

handleJoinPartRoom db toVitelity toComponent existingRoom from to tel payloads join
	| join,
	  [x] <- isNamed (fromString "{http://jabber.org/protocol/muc#user}x") =<< payloads,
	  [status] <- isNamed (fromString "{http://jabber.org/protocol/muc#user}status") =<< elementChildren x,
	  (_:_) <- code "110" status = do
		existingInvite <- tcGetJID db tel "invited"
		when (existingInvite == parseJID bareMUC) $ do
			True <- TC.runTCM $ TC.out db $ tcKey tel "invited"
			return ()
		tcPutJID db tel "joined" from
		bookmarks <- fmap (fromMaybe [] . (readZ =<<)) (TC.runTCM $ TC.get db (tcKey tel "bookmarks"))
		True <- TC.runTCM (TC.put db (tcKey tel "bookmarks") (show $ sort $ nub $ T.unpack bareMUC : bookmarks))

		creating <- tcGetJID db tel "creating"
		void $ TC.runTCM $ TC.out db $ tcKey tel "creating"
		let code201 = if fmap bareTxt creating == Just bareMUC then
				-- Hack for servers that don't support reserved rooms
				-- If we planned to create it, assume we did
				[undefined]
			else
				code "201" status

		presence <- fmap (fromMaybe [] . (readZ =<<)) (TC.runTCM $ TC.get db (T.unpack bareMUC <> "\0presence"))
		when (null code201 && not (resourceFrom `elem` presence)) $
			writeStanzaChan toVitelity $ mkSMS tel (mconcat [
				fromString "* You have joined ", bareMUC,
				fromString " as ", resourceFrom,
				fromString " along with\n",
				intercalate (fromString ", ") (filter (/= resourceFrom) presence)
			])

		queryDisco toComponent room to
	| not join,
	  [x] <- isNamed (fromString "{http://jabber.org/protocol/muc#user}x") =<< payloads,
	  (_:_) <- code "303" =<< isNamed (fromString "{http://jabber.org/protocol/muc#user}status") =<< elementChildren x = do
		presence <- fmap (fromMaybe [] . (readZ =<<)) (TC.runTCM $ TC.get db (T.unpack bareMUC <> "\0presence"))
		mapM_ (\nick -> do
			True <- TC.runTCM (TC.put db (T.unpack bareMUC <> "\0presence") (show $ sort $ nub $ nick : filter (/=resourceFrom) presence))
			writeStanzaChan toVitelity $ mkSMS tel $ mconcat [
					fromString "* ",
					resourceFrom,
					fromString " has changed their nick to ",
					nick
				]
			return ()
			) $ attributeText (fromString "nick")
				=<< listToMaybe (isNamed (fromString "{http://jabber.org/protocol/muc#user}item") =<< elementChildren x)
	| not join && existingRoom == Just from = do
		True <- TC.runTCM $ TC.out db $ tcKey tel "joined"
		writeStanzaChan toVitelity $ mkSMS tel (fromString "* You have left " <> bareMUC)
	| fmap bareTxt existingRoom == Just bareMUC = do
		presence <- fmap (fromMaybe [] . (readZ =<<)) (TC.runTCM $ TC.get db (T.unpack bareMUC <> "\0presence"))
		when (mod $ resourceFrom `elem` presence) $
			writeStanzaChan toVitelity $ mkSMS tel $ mconcat [
				fromString "* ",
				resourceFrom,
				fromString " has ",
				fromString $ if join then "joined" else "left",
				fromString " the group"
			]
	| otherwise = return ()
	where
	resourceFrom = fromMaybe mempty (strResource <$> jidResource from)
	mod = if join then not else id
	Just room = parseJID bareMUC
	bareMUC = bareTxt from

componentStanza _ _ toComponent _ (ReceivedMessage (m@Message { messageTo = Just to, messageFrom = Just from}))
	| [x] <- isNamed (fromString "{http://jabber.org/protocol/muc#user}x") =<< messagePayloads m,
	  [status] <- isNamed (fromString "{http://jabber.org/protocol/muc#user}status") =<< elementChildren x,
	  (_:_) <- code "104" status =
		queryDisco toComponent from to
componentStanza db toVitelity toComponent componentHost (ReceivedMessage (m@Message { messageTo = Just to, messageFrom = Just from}))
	| Just tel <- strNode <$> jidNode to,
	  T.length tel == 11 && fromString "1" `T.isPrefixOf` tel = do
		existingRoom <- tcGetJID db tel "joined"
		componentMessage db toVitelity m existingRoom (bareTxt from) resourceFrom tel $
			getBody "jabber:component:accept" m
	| Just jid <- (`telToJid` fromString componentHost) =<< strNode <$> jidNode to =
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
	| Just tel <- strNode <$> jidNode to = do
		let errorText = maybe mempty (mconcat . (fromString "\n":) . elementText) $ listToMaybe $
			isNamed (fromString "{urn:ietf:params:xml:ns:xmpp-stanzas}text") =<<
			elementChildren =<< isNamed (fromString "{jabber:component:accept}error") =<< presencePayloads p
		writeStanzaChan toVitelity $ mkSMS tel (fromString "* Failed to join " <> bareTxt from <> errorText)
componentStanza db toVitelity toComponent _ (ReceivedPresence (Presence {
		presenceType = typ,
		presenceFrom = Just from,
		presenceTo = Just to@(JID { jidNode = Just toNode }),
		presencePayloads = payloads
	})) | typ `elem` [PresenceAvailable, PresenceUnavailable] = do
		existingRoom <- tcGetJID db (strNode toNode) "joined"
		handleJoinPartRoom db toVitelity toComponent existingRoom from to (strNode toNode) payloads (typ == PresenceAvailable)
componentStanza _ _ toComponent _ (ReceivedPresence (Presence { presenceType = PresenceSubscribe, presenceFrom = Just from, presenceTo = Just to@JID { jidNode = Nothing } })) = do
	writeStanzaChan toComponent $ (emptyPresence PresenceSubscribed) {
		presenceTo = Just from,
		presenceFrom = Just to
	}
	writeStanzaChan toComponent $ (emptyPresence PresenceSubscribe) {
		presenceTo = Just from,
		presenceFrom = Just to
	}
componentStanza _ _ toComponent _ (ReceivedPresence (Presence { presenceType = PresenceAvailable, presenceFrom = Just from, presenceTo = Just to@JID { jidNode = Nothing } })) =
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
						(fromString "{http://jabber.org/protocol/disco#info}var", [ContentText $ fromString "http://jabber.org/protocol/muc"])
					] [],
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
				tcPutJID db tel "creating" room
				leaveRoom db toComponent componentHost tel "Joined a different room."
				joinRoom db toComponent componentHost tel room
			_ -> return () -- Invalid packet, ignore
componentStanza _ _ toComponent componentHost (ReceivedIQ (IQ { iqType = IQResult, iqFrom = Just from, iqTo = Just to }))
	| (strNode <$> jidNode to) == Just (fromString "create"),
	  Just resource <- strResource <$> jidResource to =
		case map T.unpack $ T.splitOn (fromString "|") resource of
			(tel:name:[]) -> void $ createRoom toComponent componentHost [T.unpack $ strDomain $ jidDomain from] tel (name <> "_" <> tel)
			(tel:name:servers) -> void $ createRoom toComponent componentHost servers tel name
			_ -> return () -- Invalid packet, ignore
componentStanza _ toVitelity _ _ (ReceivedIQ (iq@IQ { iqType = IQError, iqFrom = Just from, iqTo = Just to }))
	| Just tel <- strNode <$> jidNode to = do
		print iq
		writeStanzaChan toVitelity $ mkSMS tel (fromString "Error while querying or configuring " <> formatJID from)
componentStanza _ toVitelity toComponent _ (ReceivedIQ (IQ { iqType = IQResult, iqFrom = Just from, iqTo = Just to, iqID = Just id }))
	| Just tel <- strNode <$> jidNode to,
	  fromString "CHEOGRAMCREATE%" `T.isPrefixOf` id = do
		writeStanzaChan toVitelity $ mkSMS tel (mconcat [fromString "* You have created ", bareTxt from])
		queryDisco toComponent from to
componentStanza _ _ toComponent _ (ReceivedIQ (IQ { iqType = IQResult, iqFrom = Just from, iqTo = Just to, iqPayload = Just p }))
	| [query] <- isNamed (fromString "{http://jabber.org/protocol/muc#owner}query") p,
	  [form] <- isNamed (fromString "{jabber:x:data}x") =<< elementChildren query = do
		uuid <- (fmap.fmap) (fromString . UUID.toString) UUID.nextUUID
		writeStanzaChan toComponent $ (emptyIQ IQSet) {
			iqTo = Just from,
			iqFrom = Just to,
			iqID = Just $ fromString ("CHEOGRAMCREATE%" <> fromMaybe "UUIDFAIL" uuid),
			iqPayload = Just $ Element (fromString "{http://jabber.org/protocol/muc#owner}query") [] [
				NodeElement $
				fillFormField (fromString "muc#roomconfig_publicroom") (fromString "0") $
				fillFormField (fromString "muc#roomconfig_membersonly") (fromString "1")
				form { elementAttributes = [(fromString "{jabber:x:data}type", [ContentText $ fromString "submit"])] }
			]
		}
componentStanza db _ _ _ (ReceivedIQ (IQ { iqType = IQResult, iqFrom = Just from, iqPayload = Just p }))
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


storePresence db (ReceivedPresence (Presence { presenceType = PresenceUnavailable, presenceFrom = Just from })) = do
	presence <- fmap (fromMaybe [] . (readZ =<<)) (TC.runTCM $ TC.get db (T.unpack (bareTxt from) <> "\0presence"))
	True <- TC.runTCM (TC.put db (T.unpack (bareTxt from) <> "\0presence") (show $ sort $ nub $ filter (/=resourceFrom) presence))
	return ()
	where
	resourceFrom = fromMaybe "" (T.unpack . strResource <$> jidResource from)
storePresence db (ReceivedPresence (Presence { presenceType = PresenceAvailable, presenceFrom = Just from })) = do
	presence <- fmap (fromMaybe [] . (readZ =<<)) (TC.runTCM $ TC.get db (T.unpack (bareTxt from) <> "\0presence"))
	True <- TC.runTCM (TC.put db (T.unpack (bareTxt from) <> "\0presence") (show $ sort $ nub $ resourceFrom:presence))
	return ()
	where
	resourceFrom = fromMaybe "" (T.unpack . strResource <$> jidResource from)
storePresence _ _ = return ()

component db toVitelity toComponent componentHost = do
	thread <- forkXMPP $ forever $ flip catchError (liftIO . print) $ do
		stanza <- liftIO $ atomically $ readTChan toComponent
		putStanza stanza

	flip catchError (\e -> liftIO (print e >> killThread thread)) $ forever $ do
		s <- getStanza
		liftIO $ componentStanza db toVitelity toComponent componentHost s
		liftIO $ storePresence db s

telToVitelity tel
	| not $ all isDigit $ T.unpack tel = Nothing
	| T.length tel == 10 = parseJID (tel <> fromString "@sms")
	| T.length tel == 11, Just tel' <- T.stripPrefix (fromString "1") tel = parseJID (tel' <> fromString "@sms")
	| otherwise = Nothing

normalizeTel tel
	| not $ all isDigit $ T.unpack tel = Nothing
	| T.length tel == 10 = Just $ T.cons '1' tel
	| T.length tel == 11, fromString "1" `T.isPrefixOf` tel = Just tel
	| otherwise = Nothing

telToJid tel host = parseJID =<< (<> fromString "@" <> host) <$> normalizeTel tel

parseJIDrequireNode txt
	| Just _ <- jidNode =<< jid = jid
	| otherwise = Nothing
	where
	jid = parseJID txt

stripCIPrefix prefix str
	| CI.mk prefix' == prefix = Just rest
	| otherwise = Nothing
	where
	(prefix', rest) = T.splitAt (T.length $ CI.original prefix) str

data Command = Help | Create Text | Join JID | JoinInvited | Send Text | Who | List | Leave | InviteCmd JID | SetNick Text | Whisper JID Text
	deriving (Show, Eq)

parseCommand txt room nick componentHost
	| Just jid <- stripCIPrefix (fromString "/invite ") txt =
		InviteCmd <$> (
			parseJIDrequireNode jid <|>
			telToJid jid (fromString componentHost)
		)
	| Just room <- stripCIPrefix (fromString "/join ") txt =
		Join <$> (parseJID (room <> fromString "/" <> nick) <|> parseJID room)
	| Just t <- stripCIPrefix (fromString "/create ") txt = Just $ Create t
	| Just nick <- stripCIPrefix (fromString "/nick ") txt = Just $ SetNick nick
	| Just input <- stripCIPrefix (fromString "/msg ") txt =
		let (to, msg) = T.breakOn (fromString " ") input in
		Whisper <$> (
			parseJIDrequireNode to <|>
			telToJid to (fromString componentHost) <|>
			(parseJID =<< fmap (\r -> bareTxt r <> fromString "/" <> to) room)
		) <*> pure msg
	| citxt == fromString "/join" = Just JoinInvited
	| citxt == fromString "/leave" = Just Leave
	| citxt == fromString "/part" = Just Leave
	| citxt == fromString "/who" = Just Who
	| citxt == fromString "/list" = Just List
	| citxt == fromString "/help" = Just Help
	| otherwise = Just $ Send txt
	where
	citxt = CI.mk txt

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
	password <- TC.runTCM $ TC.get db (tcKey tel (T.unpack (bareTxt room) <> "\0muc_roomsecret"))
	let pwEl = maybe [] (\pw -> [
			NodeElement $ Element (fromString "{http://jabber.org/protocol/muc}password") [] [NodeContent $ ContentText $ fromString pw]
		]) password

	writeStanzaChan toComponent $ (emptyPresence PresenceAvailable) {
		presenceTo = Just room,
		presenceFrom = telToJid tel (fromString componentHost),
		presencePayloads = [Element (fromString "{http://jabber.org/protocol/muc}x") [] ([
			NodeElement $ Element (fromString "{http://jabber.org/protocol/muc}history") [(fromString "{http://jabber.org/protocol/muc}maxchars", [ContentText $ fromString "0"])] []
		] <> pwEl)]
	}

createRoom :: TChan StanzaRec -> String -> [String] -> String -> String -> IO Bool
createRoom toComponent componentHost (server:otherServers) tel name =
	-- First we check if this room exists on the server already
	case to of
		Just t -> queryDisco toComponent t jid >> return True
		Nothing -> return False
	where
	to = parseJID $ fromString $ name <> "@" <> server
	Just jid = parseJID $ fromString $ "create@" <> componentHost <> "/" <> intercalate "|" (tel:name:otherServers)
createRoom _ _ [] _ _ = return False

mucShortMatch tel short muc =
	node == short || T.stripSuffix (fromString "_" <> tel) node == Just short
	where
	node = maybe mempty strNode (jidNode =<< parseJID muc)

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
			unless validRoom $
				writeStanzaChan toVitelity $ mkSMS tel (fromString "Invalid group name")
		Just (Join room) -> do
			leaveRoom db toComponent componentHost tel "Joined a different room."
			bookmarks <- fmap (fromMaybe [] . (readZ =<<)) (TC.runTCM $ TC.get db (tcKey tel "bookmarks"))
			joinRoom db toComponent componentHost tel $
				fromMaybe room $ parseJID =<< fmap (<> fromString "/" <> nick)
				(find (mucShortMatch tel (strDomain $ jidDomain room)) bookmarks)
		Just Leave -> leaveRoom db toComponent componentHost tel "Typed /leave"
		Just Who -> do
			let snick = T.unpack nick
			let room = maybe "" (T.unpack . bareTxt) existingRoom
			presence <- fmap (fromMaybe [] . (readZ =<<)) (TC.runTCM $ TC.get db (room <> "\0presence"))
			writeStanzaChan toVitelity $ mkSMS tel $ fromString $ mconcat [
					"You are joined to ", room,
					" as ", snick,
					" along with\n",
					intercalate ", " (filter (/= snick) presence)
				]
		Just List -> do
			bookmarks <- fmap (fromMaybe [] . (readZ =<<)) (TC.runTCM $ TC.get db (tcKey tel "bookmarks"))
			writeStanzaChan toVitelity $ mkSMS tel $ fromString $ "Groups you can /join\n" <> intercalate "\n" bookmarks
		Just (InviteCmd jid)
			| Just room <- existingRoom -> do
				membersonly <- maybe False toEnum <$> TC.runTCM (TC.get db (T.unpack (bareTxt room) <> "\0muc_membersonly"))
				when membersonly $ do
					-- Try to add everyone we invite as an owner also
					uuid <- (fmap.fmap) UUID.toString UUID.nextUUID
					writeStanzaChan toComponent $ (emptyIQ IQSet) {
						iqTo = Just room,
						iqFrom = telToJid tel (fromString componentHost),
						iqID = fmap fromString uuid,
						iqPayload = Just $ Element (fromString "{http://jabber.org/protocol/muc#admin}admin") [] [
							NodeElement $ Element (fromString "{http://jabber.org/protocol/muc#admin}item")
								[
									(fromString "{http://jabber.org/protocol/muc#admin}affiliation", [ContentText $ fromString "owner"]),
									(fromString "{http://jabber.org/protocol/muc#admin}jid", [ContentText $ formatJID jid])
								] []
						]
					}

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
			| otherwise -> writeStanzaChan toVitelity $ mkSMS tel (fromString "You are not joined to a group. Reply with /help to learn more")
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
			| fromString "(SMSSERVER) " `T.isPrefixOf` msg -> return () -- bogus message from vitelity, ignore
			| Just room <- existingRoom -> sendToRoom toComponent componentHost tel room msg
			| otherwise -> writeStanzaChan toVitelity $ mkSMS tel (fromString "You are not joined to a group")
		Just Help -> do
			writeStanzaChan toVitelity $ mkSMS tel $ fromString $ mconcat [
					"Invite to group: /invite phone-number\n",
					"Show group participants: /who\n",
					"Set nick: /nick nickname\n",
					"List groups: /list\n",
					"Create a group: /create short-name"
				]
			writeStanzaChan toVitelity $ mkSMS tel $ fromString $ mconcat [
					"Join existing group: /join group-name\n",
					"Whisper to user: /msg username message\n",
					"Leave group: /leave\n",
					"More info: http://cheogram.com"
				]
		Nothing -> writeStanzaChan toVitelity $ mkSMS tel (fromString "You sent an invalid message")

viteltiy db chunks toVitelity toComponent componentHost conferenceServers = do
	putStanza $ emptyPresence PresenceAvailable

	thread <- forkXMPP $ forever $ flip catchError (liftIO . print) $ do
		wait <- liftIO $ getStdRandom (randomR (400000,1500000))
		stanza <- liftIO $ atomically $ readTChan toVitelity
		forM_ (strNode <$> (jidNode =<< stanzaTo stanza)) $ \tel -> do
			welcomed <- maybe False toEnum <$> liftIO (TC.runTCM $ TC.get db $ tcKey tel "welcomed")
			unless welcomed $ do
				putStanza $ mkSMS tel $ fromString "Welcome to CheoGram! You can chat with groups of friends (one at a time), by replying to this number. Reply with /help to learn more or visit cheogram.com"
				True <- liftIO (TC.runTCM $ TC.put db (tcKey tel "welcomed") (fromEnum True))
				liftIO $ threadDelay wait

		putStanza stanza
		liftIO $ threadDelay wait

	flip catchError (\e -> liftIO (print e >> killThread thread)) $ forever $ do
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

	void $ forkIO $ forever $ threadDelay 1500000 >> atomically (writeTChan chunks TimerExpire)
	void $ forkIO $ multipartStitcher db chunks toVitelity toComponent name conferences

	void $ forkIO $ forever $ print =<< runComponent (Server (fromString name) host (PortNumber $ fromIntegral (read port :: Int))) (fromString secret) (component db toVitelity toComponent name)

	let Just vitelityParsedJid = parseJID $ fromString vitelityJid
	forever $ runClient (Server (fromString "s.ms") "s.ms" (PortNumber 5222)) vitelityParsedJid (fromMaybe mempty $ strNode <$> jidNode vitelityParsedJid) (fromString vitelityPassword) $ do
		void $ bindJID vitelityParsedJid
		viteltiy db chunks toVitelity toComponent name conferences
