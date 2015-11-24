{-# LANGUAGE PackageImports #-}
import System.Environment
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

code110 status =
	hasAttributeText (fromString "{http://jabber.org/protocol/muc#user}code") (== (fromString "110")) status
	<>
	hasAttributeText (fromString "code") (== (fromString "110")) status

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
				fromString ". You can switch to this chat by sending\n\n/join ",
				formatJID (inviteMUC invite)
			]
		when (existingRoom /= Just (inviteMUC invite) && existingInvite /= Just (inviteMUC invite)) $ do
			tcPutJID db tel "invited" (inviteMUC invite)
			writeStanzaChan toVitelity $ mkSMS tel txt
componentStanza db toVitelity _ (ReceivedMessage (m@Message { messageTo = Just to, messageFrom = Just from}))
	| Just tel <- strNode <$> jidNode to,
	  Just body <- getBody "jabber:component:accept" m = do
		existingRoom <- tcGetJID db tel "joined"
		componentMessage db toVitelity (messageType m) (fromMaybe mempty $ messageID m) existingRoom (bareTxt from) resourceFrom tel body
	where
	resourceFrom = strResource <$> jidResource from
componentStanza db toVitelity _ (ReceivedPresence p@(Presence { presenceType = PresenceAvailable, presenceFrom = Just from, presenceTo = Just to }))
	| Just tel <- strNode <$> jidNode to,
	  [x] <- isNamed (fromString "{http://jabber.org/protocol/muc#user}x") =<< presencePayloads p,
	  [status] <- isNamed (fromString "{http://jabber.org/protocol/muc#user}status") =<< elementChildren x,
	  (_:_) <- code110 status = do
		tcPutJID db tel "joined" from
		writeStanzaChan toVitelity $ mkSMS tel (mconcat [fromString "* You have joined ", bareMUC, fromString " as ", roomNick])
	where
	bareMUC = bareTxt from
	roomNick = fromMaybe mempty (strResource <$> jidResource from)
componentStanza db toVitelity _ (ReceivedPresence p@(Presence { presenceType = PresenceUnavailable, presenceFrom = Just from, presenceTo = Just to }))
	| Just tel <- strNode <$> jidNode to = do
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
	forkXMPP $ forever $ flip catchError (const $ return ()) $ do
		stanza <- liftIO $ atomically $ readTChan toComponent
		putStanza $ stanza

	--forever $ getStanza >>= liftIO . componentStanza db toVitelity
	forever $ flip catchError (const $ return ()) $ do
		s <- getStanza
		liftIO $ componentStanza db toVitelity toComponent s

data Command = Join JID | Send Text
	deriving (Show, Eq)

parseCommand txt nick
	| Just room <- T.stripPrefix (fromString "/join ") txt =
		Join <$> parseJID (room <> fromString "/" <> nick)
	| otherwise = Just $ Send txt

getMessage (ReceivedMessage m) = Just m
getMessage _ = Nothing

viteltiy db toVitelity toComponent = do
	putStanza $ emptyPresence PresenceAvailable

	forkXMPP $ forever $ flip catchError (const $ return ()) $ do
		stanza <- liftIO $ atomically $ readTChan toVitelity
		putStanza $ stanza

	forever $ flip catchError (const $ return ()) $ do
		m <- getMessage <$> getStanza
		liftIO $ case (strNode <$> (jidNode =<< messageFrom =<< m), getBody "jabber:client" =<< m) of
			(Just tel, Just txt) -> case parseCommand txt tel of
					Just (Join room) -> do
						existingRoom <- tcGetJID db tel "joined"
						forM_ existingRoom $ \leaveRoom -> do
							writeStanzaChan toComponent $ (emptyPresence PresenceUnavailable) {
								presenceTo = Just leaveRoom,
								presenceFrom = parseJID $ tel <> fromString "@sms.singpolyma.net",
								presencePayloads = [Element (fromString "{jabber:component:accept}status") [] [NodeContent $ ContentText $ fromString "Joined a different room."]]
							}
							True <- TC.runTCM $ TC.out db $ tcKey tel "joined"
							return ()

						writeStanzaChan toComponent $ (emptyPresence PresenceAvailable) {
							presenceTo = Just room,
							presenceFrom = parseJID $ tel <> fromString "@sms.singpolyma.net",
							presencePayloads = [Element (fromString "{http://jabber.org/protocol/muc}x") [] [
								NodeElement $ Element (fromString "{http://jabber.org/protocol/muc}history") [(fromString "{http://jabber.org/protocol/muc}maxchars", [ContentText $ fromString "0"])] []
							]]
						}
					Just (Send msg) -> do
						existingRoom <- tcGetJID db tel "joined"
						case existingRoom of
							Just room -> do
								uuid <- (fmap.fmap) UUID.toString UUID.nextUUID
								writeStanzaChan toComponent $ (emptyMessage MessageGroupChat) {
									messageTo = parseJID $ bareTxt room,
									messageFrom = parseJID $ tel <> fromString "@sms.singpolyma.net",
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
		viteltiy db toVitelity toComponent
