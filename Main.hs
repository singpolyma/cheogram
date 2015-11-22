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

getBody ns = listToMaybe . fmap (mconcat . elementText) . (isNamed (Name (fromString "body") (Just $ fromString ns) Nothing) <=< messagePayloads)

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
	   not (fromString "GROUPSMS%" `T.isPrefixOf` mid)) then
		writeStanzaChan toVitelity $ (emptyMessage MessageChat) {
			messageTo = parseJID (tel <> fromString "@sms"),
			messagePayloads = [Element (fromString "{jabber:client}body") [] [NodeContent $ ContentText txt]]
		}
	else
		return () -- TODO: Error?
	where
	txt = mconcat [fromString "(", fromMaybe (fromString "nonick") resourceFrom, fromString ") ", body]
componentMessage _ toVitelity _ _ existingRoom bareFrom resourceFrom tel body =
	writeStanzaChan toVitelity ((emptyMessage MessageChat) {
		messageTo = parseJID (tel <> fromString "@sms"),
		messagePayloads = [Element (fromString "{jabber:client}body") [] [NodeContent $ ContentText txt]]
	})
	where
	txt = mconcat [fromString "(", fromNick, fromString " whispers) ", body]
	fromNick
		| fmap bareTxt existingRoom == Just bareFrom = fromMaybe (fromString "nonick") resourceFrom
		| otherwise = bareFrom

componentStanza db toVitelity (ReceivedMessage (m@Message { messageTo = Just to, messageFrom = Just from}))
	| Just tel <- strNode <$> jidNode to,
	  Just body <- getBody "jabber:component:accept" m = do
		existingRoom <- (parseJID . fromString =<<) <$> TC.runTCM (TC.get db $ T.unpack tel)
		componentMessage db toVitelity (messageType m) (fromMaybe mempty $ messageID m) existingRoom (bareTxt from) resourceFrom tel body
	where
	resourceFrom = strResource <$> jidResource from
componentStanza db toVitelity (ReceivedPresence p@(Presence { presenceFrom = Just from, presenceTo = Just to }))
	| Just tel <- strNode <$> jidNode to,
	  [x] <- isNamed (fromString "{http://jabber.org/protocol/muc#user}x") =<< presencePayloads p,
	  [status] <- isNamed (fromString "{http://jabber.org/protocol/muc#user}status") =<< elementChildren x,
	  (_:_) <- code110 status = do
		writeStanzaChan toVitelity $ (emptyMessage MessageChat) {
			messageTo = parseJID (tel <> fromString "@sms"),
			messagePayloads = [Element (fromString "{jabber:client}body") [] [NodeContent $ ContentText $ fromString "You have joined " <> bareMUC <> fromString " as " <> roomNick]]
		}

		True <- TC.runTCM (TC.put db (T.unpack tel) (T.unpack $ formatJID from))
		return ()
	where
	bareMUC = bareTxt from
	roomNick = fromMaybe mempty (strResource <$> jidResource from)
componentStanza _ _ _ = return ()

component db toVitelity toComponent = do
	forkXMPP $ forever $ flip catchError (const $ return ()) $ do
		stanza <- liftIO $ atomically $ readTChan toComponent
		putStanza $ stanza

	--forever $ getStanza >>= liftIO . componentStanza db toVitelity
	forever $ flip catchError (const $ return ()) $ do
		s <- getStanza
		liftIO $ componentStanza db toVitelity s

data Command = Join JID | Send Text
	deriving (Show, Eq)

parseCommand txt nick
	| Just room <- T.stripPrefix (fromString "/join ") txt =
		Join <$> parseJID (room <> fromString "/" <> nick)
	| otherwise = Just $ Send txt

getMessage (ReceivedMessage m) = Just m
getMessage _ = Nothing

viteltiy db toVitelity toComponent = do
	bindJID (fromString "2266669991@s.ms/theone")
	putStanza $ emptyPresence PresenceAvailable

	forkXMPP $ forever $ flip catchError (const $ return ()) $ do
		stanza <- liftIO $ atomically $ readTChan toVitelity
		putStanza $ stanza

	forever $ flip catchError (const $ return ()) $ do
		m <- getMessage <$> getStanza
		liftIO $ case (strNode <$> (jidNode =<< messageFrom =<< m), getBody "jabber:client" =<< m) of
			(Just tel, Just txt) -> case parseCommand txt (fromString "thenick") of
					Just (Join room) -> do
						existingRoom <- (parseJID . fromString =<<) <$> TC.runTCM (TC.get db $ T.unpack tel)
						forM_ existingRoom $ \leaveRoom ->
							writeStanzaChan toComponent $ (emptyPresence PresenceUnavailable) {
								presenceTo = Just leaveRoom,
								presenceFrom = parseJID $ tel <> fromString "@sms.singpolyma.net",
								presencePayloads = [Element (fromString "{jabber:component:accept}status") [] [NodeContent $ ContentText $ fromString "Joined a different room."]]
							}

						writeStanzaChan toComponent $ (emptyPresence PresenceAvailable) {
							presenceTo = Just room,
							presenceFrom = parseJID $ tel <> fromString "@sms.singpolyma.net",
							presencePayloads = [Element (fromString "{http://jabber.org/protocol/muc}x") [] [
								NodeElement $ Element (fromString "{http://jabber.org/protocol/muc}history") [(fromString "{http://jabber.org/protocol/muc}maxchars", [ContentText $ fromString "0"])] []
							]]
						}
					Just (Send msg) -> do
						existingRoom <- (parseJID . fromString =<<) <$> TC.runTCM (TC.get db $ T.unpack tel)
						case existingRoom of
							Just room -> do
								uuid <- (fmap.fmap) UUID.toString UUID.nextUUID
								writeStanzaChan toComponent $ (emptyMessage MessageGroupChat) {
									messageTo = parseJID $ bareTxt room,
									messageFrom = parseJID $ tel <> fromString "@sms.singpolyma.net",
									messageID = Just $ fromString ("GROUPSMS%" <> fromMaybe "UUIDFAIL" uuid),
									messagePayloads = [Element (fromString "{jabber:component:accept}body") [] [NodeContent $ ContentText msg]]
								}
							Nothing -> print "ERROR"
					Nothing -> print "ERROR"
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
	runClient (Server (fromString "s.ms") "s.ms" (PortNumber 5222)) vitelityParsedJid (fromMaybe mempty $ strNode <$> jidNode vitelityParsedJid) (fromString vitelityPassword) $ viteltiy db toVitelity toComponent
