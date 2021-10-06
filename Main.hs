{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NamedFieldPuns #-}
import Prelude (show, read)
import BasicPrelude hiding (show, read, forM, mapM, forM_, mapM_, getArgs, log)
import System.IO (stdout, stderr, hSetBuffering, BufferMode(LineBuffering))
import Data.Char
import Control.Concurrent
import Control.Concurrent.STM
import Data.Foldable (forM_, mapM_, toList)
import Data.Traversable (forM, mapM)
import System.Environment (getArgs)
import Control.Error (readZ, MaybeT(..), hoistMaybe, headZ, justZ, hush)
import Data.Time (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Network (PortID(PortNumber))
import Network.URI (parseURI, uriPath, escapeURIString)
import System.Random (Random(randomR), getStdRandom)
import System.Random.Shuffle (shuffleM)
import Data.Digest.Pure.SHA (sha1, bytestringDigest, showDigest)
import Network.StatsD (openStatsD)
import qualified Network.StatsD as StatsD

import "monads-tf" Control.Monad.Error (catchError) -- ick
import Data.XML.Types as XML (Element(..), Node(NodeContent, NodeElement), Content(ContentText), isNamed, hasAttributeText, elementText, elementChildren, attributeText, attributeContent, hasAttribute, nameNamespace)
import UnexceptionalIO (Unexceptional, UIO)
import qualified UnexceptionalIO as UIO
import qualified Dhall
import qualified Jingle
import qualified Jingle.StoreChunks as Jingle
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Map as Map
import qualified Data.UUID as UUID ( toString )
import qualified Data.UUID.V1 as UUID ( nextUUID )
import qualified Data.ByteString.Lazy as LZ
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Builder as Builder
import qualified Database.TokyoCabinet as TC
import qualified Database.Redis as Redis
import qualified Text.Regex.PCRE.Light as PCRE
import qualified Network.Http.Client as HTTP
import qualified System.IO.Streams as Streams
import Network.Protocol.XMPP as XMPP -- should import qualified

import Util
import IQManager
import qualified ConfigureDirectMessageRoute
import qualified Config
import Adhoc (adhocBotSession, commandList, queryCommandList)
import StanzaRec

instance Ord JID where
	compare x y = compare (show x) (show y)

tcKey jid key = fmap (\node -> (T.unpack $ strNode node) <> "\0" <> key) (jidNode jid)
tcGetJID db jid key = liftIO $ case tcKey jid key of
	Just tck -> (parseJID . fromString =<<) <$> TC.runTCM (TC.get db tck)
	Nothing -> return Nothing
tcPutJID db cheoJid key jid = tcPut db cheoJid key $ T.unpack $ formatJID jid
tcPut db cheoJid key val = liftIO $ do
	let Just tck = tcKey cheoJid key
	True <- TC.runTCM (TC.put db tck val)
	return ()

queryDisco to from = (:[]) . mkStanzaRec <$> queryDiscoWithNode Nothing to from

queryDiscoWithNode node to from = do
	uuid <- (fmap.fmap) (fromString . UUID.toString) UUID.nextUUID
	return $ (queryDiscoWithNode' node to from) {
		iqID = uuid
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

nickFor db jid existingRoom
	| fmap bareTxt existingRoom == Just bareFrom = return $ fromMaybe (fromString "nonick") resourceFrom
	| Just tel <- mfilter isE164 (strNode <$> jidNode jid) = do
		mnick <- maybe (return Nothing) (TC.runTCM .TC.get db) (tcKey jid "nick")
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

cheogramDiscoInfo db componentJid sendIQ from q = do
	canVoice <- isJust <$> getSipProxy db componentJid sendIQ from
	return $ Element (fromString "{http://jabber.org/protocol/disco#info}query")
		(map (\node -> (s"{http://jabber.org/protocol/disco#info}node", [ContentText node])) $ maybeToList $ nodeAttribute =<< q)
		(catMaybes [
			Just $ NodeElement $ Element (fromString "{http://jabber.org/protocol/disco#info}identity") [
				(fromString "{http://jabber.org/protocol/disco#info}category", [ContentText $ fromString "gateway"]),
				(fromString "{http://jabber.org/protocol/disco#info}type", [ContentText $ fromString "sms"]),
				(fromString "{http://jabber.org/protocol/disco#info}name", [ContentText $ fromString "Cheogram"])
				] [],
			mfilter (const canVoice) $ Just $ NodeElement $ Element (fromString "{http://jabber.org/protocol/disco#info}identity") [
				(fromString "{http://jabber.org/protocol/disco#info}category", [ContentText $ fromString "gateway"]),
				(fromString "{http://jabber.org/protocol/disco#info}type", [ContentText $ fromString "pstn"]),
				(fromString "{http://jabber.org/protocol/disco#info}name", [ContentText $ fromString "Cheogram"])
			] [],
			Just $ NodeElement $ Element (fromString "{http://jabber.org/protocol/disco#info}feature") [
				(fromString "{http://jabber.org/protocol/disco#info}var", [ContentText $ fromString "http://jabber.org/protocol/commands"])
			] [],
			Just $ NodeElement $ Element (fromString "{http://jabber.org/protocol/disco#info}feature") [
				(fromString "{http://jabber.org/protocol/disco#info}var", [ContentText $ fromString "jabber:iq:gateway"])
			] [],
			Just $ NodeElement $ Element (fromString "{http://jabber.org/protocol/disco#info}feature") [
				(fromString "{http://jabber.org/protocol/disco#info}var", [ContentText $ fromString "jabber:iq:register"])
			] [],
			Just $ NodeElement $ Element (fromString "{http://jabber.org/protocol/disco#info}feature") [
				(fromString "{http://jabber.org/protocol/disco#info}var", [ContentText $ fromString "urn:xmpp:ping"])
			] [],
			Just $ NodeElement $ Element (fromString "{http://jabber.org/protocol/disco#info}feature") [
				(fromString "{http://jabber.org/protocol/disco#info}var", [ContentText $ fromString "vcard-temp"])
			] []
		])

cheogramAvailable db componentJid sendIQ from to = do
	disco <- cheogramDiscoInfo db componentJid sendIQ to Nothing
	let ver = T.decodeUtf8 $ Base64.encode $ discoToCapsHash disco
	return $ (emptyPresence PresenceAvailable) {
			presenceTo = Just to,
			presenceFrom = Just from,
			presencePayloads = [
				Element (s"{http://jabber.org/protocol/caps}c") [
					(s"{http://jabber.org/protocol/caps}hash", [ContentText $ fromString "sha-1"]),
					(s"{http://jabber.org/protocol/caps}node", [ContentText $ fromString "xmpp:cheogram.com"]),
					(s"{http://jabber.org/protocol/caps}ver", [ContentText ver])
				] []
			]
		}

telDiscoFeatures = [
		s"http://jabber.org/protocol/muc",
		s"jabber:x:conference",
		s"urn:xmpp:ping",
		s"urn:xmpp:receipts",
		s"vcard-temp",
		s"urn:xmpp:jingle:1",
		s"urn:xmpp:jingle:apps:file-transfer:3",
		s"urn:xmpp:jingle:apps:file-transfer:5",
		s"urn:xmpp:jingle:transports:s5b:1",
		s"urn:xmpp:jingle:transports:ibb:1"
	]

getSipProxy :: TC.HDB -> JID -> (IQ -> UIO (STM (Maybe IQ))) -> JID -> IO (Maybe Text)
getSipProxy db componentJid sendIQ jid = do
	maybeProxy <- TC.runTCM $ TC.get db $ T.unpack (bareTxt jid) ++ "\0sip-proxy"
	case maybeProxy of
		Just proxy -> return $ Just $ T.pack proxy
		Nothing ->
			(extractSip =<<) <$> routeQueryStateful db componentJid sendIQ jid Nothing query
	where
	query jidTo jidFrom = return $ (emptyIQ IQGet) {
			iqTo = Just jidTo,
			iqFrom = Just jidFrom,
			iqPayload = Just $ XML.Element (s"{urn:xmpp:extdisco:2}services") [
				(s"type", [XML.ContentText $ s"sip"])
			] []
		}
	extractSip (IQ { iqPayload = payload }) =
		headZ $
		(mapMaybe (attributeText (s"host")) $
		filter (\el -> attributeText (s"type") el == Just (s"sip")) $
		isNamed (s"{urn:xmpp:extdisco:2}service") =<<
		elementChildren =<< (justZ payload))

getTelFeatures db componentJid sendIQ jid = do
	maybeProxy <- getSipProxy db componentJid sendIQ jid
	log "TELFEATURES" (jid, maybeProxy)
	return $ maybe [] (const $ [s"urn:xmpp:jingle:transports:ice-udp:1", s"urn:xmpp:jingle:apps:dtls:0", s"urn:xmpp:jingle:apps:rtp:1", s"urn:xmpp:jingle:apps:rtp:audio", s"urn:xmpp:jingle-message:0"]) maybeProxy

telCapsStr extraVars =
	s"client/sms//Cheogram<" ++ mconcat (intersperse (s"<") (sort (nub (telDiscoFeatures ++ extraVars)))) ++ s"<"

telAvailable from to disco =
	(emptyPresence PresenceAvailable) {
		presenceTo = Just to,
		presenceFrom = Just fromWithResource,
		presencePayloads = [
			Element (s"{http://jabber.org/protocol/caps}c") [
				(s"{http://jabber.org/protocol/caps}hash", [ContentText $ fromString "sha-1"]),
				(s"{http://jabber.org/protocol/caps}node", [ContentText $ fromString "xmpp:cheogram.com"]),
				(s"{http://jabber.org/protocol/caps}ver", [ContentText hash])
			] []
		]
	}
	where
	fromWithResource
		| Nothing <- jidResource from,
		  Just newFrom <- parseJID (bareTxt from ++ s"/tel") = newFrom
		| otherwise = from
	hash = T.decodeUtf8 $ Base64.encode $ LZ.toStrict $ bytestringDigest $ sha1 $ LZ.fromStrict $ T.encodeUtf8 $ telCapsStr disco

nodeAttribute el =
	attributeText (s"{http://jabber.org/protocol/disco#info}node") el <|>
	attributeText (s"node") el

telDiscoInfo q id from to disco =
	(emptyIQ IQResult) {
		iqTo = Just to,
		iqFrom = Just from,
		iqID = Just id,
		iqPayload = Just $ Element (fromString "{http://jabber.org/protocol/disco#info}query")
			(map (\node -> (s"{http://jabber.org/protocol/disco#info}node", [ContentText node])) $ maybeToList $ nodeAttribute q) $
			[
				NodeElement $ Element (s"{http://jabber.org/protocol/disco#info}identity") [
					(s"{http://jabber.org/protocol/disco#info}category", [ContentText $ s"client"]),
					(s"{http://jabber.org/protocol/disco#info}type", [ContentText $ s"sms"]),
					(s"{http://jabber.org/protocol/disco#info}name", [ContentText $ s"Cheogram"])
				] []
			] ++ map (\var ->
				NodeElement $ Element (fromString "{http://jabber.org/protocol/disco#info}feature") [
					(fromString "{http://jabber.org/protocol/disco#info}var", [ContentText var])
				] []
			) (sort $ nub $ telDiscoFeatures ++ disco)
	}

routeQueryOrReply db componentJid from smsJid resource query reply = do
	maybeRoute <- TC.runTCM $ TC.get db (T.unpack (bareTxt from) ++ "\0direct-message-route")
	case (fmap fromString maybeRoute, maybeRouteFrom) of
		(Just route, Just routeFrom) ->
				let routeTo = fromMaybe componentJid $ parseJID $ (maybe mempty (++ s"@") $ strNode <$> jidNode smsJid) ++ route in
				query routeTo routeFrom
		_ -> return [mkStanzaRec $ reply]
	where
	maybeRouteFrom = parseJID $ escapeJid (bareTxt from) ++ s"@" ++ formatJID componentJid ++ s"/" ++ (fromString resource)

routeQueryStateful db componentJid sendIQ from targetNode query = do
	maybeRoute <- TC.runTCM $ TC.get db (T.unpack (bareTxt from) ++ "\0direct-message-route")
	case (fmap fromString maybeRoute, maybeRouteFrom) of
		(Just route, Just routeFrom) -> do
			let Just routeTo = parseJID $ (maybe mempty (++ s"@") $ strNode <$> targetNode) ++ route
			iqToSend <- query routeTo routeFrom
			result <- atomicUIO =<< UIO.lift (sendIQ iqToSend)
			return $ mfilter ((==IQResult) . iqType) result
		_ -> return Nothing
	where
	maybeRouteFrom = parseJID $ escapeJid (bareTxt from) ++ s"@" ++ formatJID componentJid ++ s"/IQMANAGER"

routeDiscoStateful db componentJid sendIQ from targetNode node =
	routeQueryStateful db componentJid sendIQ from targetNode (queryDiscoWithNode node)

routeDiscoOrReply db componentJid from smsJid resource node reply =
	routeQueryOrReply db componentJid from smsJid resource (fmap (pure . mkStanzaRec) .: queryDiscoWithNode node) reply

deliveryReceipt id from to =
	(emptyMessage MessageNormal) {
		messageFrom = Just from,
		messageTo = Just to,
		messagePayloads = [
			Element (s"{urn:xmpp:receipts}received")
				[(s"{urn:xmpp:receipts}id", [ContentText id])] []
		]
	}

iqNotImplemented iq =
	iq {
		iqTo = iqFrom iq,
		iqFrom = iqTo iq,
		iqType = IQError,
		iqPayload = Just $ Element (s"{jabber:component:accept}error")
			[(s"{jabber:component:accept}type", [ContentText $ fromString "cancel"])]
			[NodeElement $ Element (s"{urn:ietf:params:xml:ns:xmpp-stanzas}feature-not-implemented") [] []]
	}

stripOptionalSuffix suffix text =
	fromMaybe text $ T.stripSuffix suffix text

-- https://otr.cypherpunks.ca/Protocol-v3-4.0.0.html
stripOtrWhitespaceOnce body =
	foldl' (\body' suffix -> stripOptionalSuffix suffix body') body [
		s"\x20\x20\x09\x09\x20\x20\x09\x09",
		s"\x20\x20\x09\x09\x20\x20\x09\x20",
		s"\x20\x09\x20\x09\x20\x20\x09\x20",
		s"\x20\x09\x20\x20\x09\x09\x09\x09",
		s"\x20\x09\x20\x09\x20\x09\x20\x20"
	]

stripOtrWhitespace = stripOtrWhitespaceOnce . stripOtrWhitespaceOnce . stripOtrWhitespaceOnce . stripOtrWhitespaceOnce . stripOtrWhitespaceOnce

mapBody f (m@Message { messagePayloads = payloads }) =
	m { messagePayloads =
		map (\payload ->
			case isNamed (s"{jabber:component:accept}body") payload of
				[] -> payload
				_ -> payload { elementNodes = [NodeContent $ ContentText $ f (concat (elementText payload))] }
		) payloads
	}

unregisterDirectMessageRoute db componentJid userJid route = do
	maybeCheoJid <- (parseJID . fromString =<<) <$> TC.runTCM (TC.get db (T.unpack (bareTxt userJid) ++ "\0cheoJid"))
	forM_ maybeCheoJid $ \cheoJid -> do
		TC.runTCM $ TC.out db (T.unpack (bareTxt userJid) ++ "\0cheoJid")

		owners <- (fromMaybe [] . (readZ =<<)) <$>
			maybe (return Nothing) (TC.runTCM . TC.get db) (tcKey cheoJid "owners")
		tcPut db cheoJid "owners" (show $ (filter (/= bareTxt userJid)) owners)

	uuid <- (fmap.fmap) (fromString . UUID.toString) UUID.nextUUID
	return $ (emptyIQ IQSet) {
			iqTo = Just route,
			iqFrom = parseJID $ escapeJid (bareTxt userJid) ++ s"@" ++ formatJID componentJid ++ s"/CHEOGRAM%removed",
			iqID = uuid,
			iqPayload = Just $ Element (s"{jabber:iq:register}query") [] [
				NodeElement $ Element (s"{jabber:iq:register}remove") [] []
			]
		}

toRouteOrFallback db componentJid bareFrom resourceFrom smsJid m fallback = do
	maybeRoute <- TC.runTCM $ TC.get db (T.unpack bareFrom ++ "\0direct-message-route")
	case (fmap fromString maybeRoute, parseJID $ escapeJid bareFrom ++ s"@" ++ formatJID componentJid ++ resourceSuffix) of
		(Just route, Just routeFrom) -> do
			return [mkStanzaRec $ m {
				messageFrom = Just routeFrom,
				messageTo = parseJID $ (fromMaybe mempty $ strNode <$> jidNode smsJid) ++ s"@" ++ route
			}]
		_ -> fallback
	where
	resourceSuffix = maybe mempty (s"/"++) resourceFrom

componentMessage db componentJid (m@Message { messageType = MessageError }) _ bareFrom resourceFrom smsJid body = do
	log "MESSAGE ERROR"  m
	toRouteOrFallback db componentJid bareFrom resourceFrom smsJid m $ do
		log "DIRECT FROM GATEWAY" smsJid
		return [mkStanzaRec $ m { messageTo = Just smsJid, messageFrom = Just componentJid }]
componentMessage db componentJid m@(Message { messageTo = Just to }) existingRoom _ _ smsJid _
	| Just invite <- getMediatedInvitation m <|> getDirectInvitation m = do
		forM_ (invitePassword invite) $ \password ->
			tcPut db to (T.unpack (formatJID $ inviteMUC invite) <> "\0muc_roomsecret") (T.unpack password)
		existingInvite <- tcGetJID db to "invited"
		nick <- nickFor db (inviteFrom invite) existingRoom
		let txt = mconcat [
				fromString "* ",
				nick,
				fromString " has invited you to a group",
				maybe mempty (\t -> fromString ", saying \"" <> t <> fromString "\"") (inviteText invite),
				fromString "\nYou can switch to this group by replying with /join"
			]
		if (existingRoom /= Just (inviteMUC invite) && existingInvite /= Just (inviteMUC invite)) then do
			tcPutJID db to "invited" (inviteMUC invite)
			regJid <- tcGetJID db to "registered"
			fmap (((mkStanzaRec $ mkSMS componentJid smsJid txt):) . concat . toList)
				(forM regJid $ \jid -> sendInvite db jid (invite { inviteFrom = to }))
		else
			return []
componentMessage _ componentJid (m@Message { messageType = MessageGroupChat }) existingRoom bareFrom resourceFrom smsJid (Just body) = do
	if fmap bareTxt existingRoom == Just bareFrom && (
	   existingRoom /= parseJID (bareFrom <> fromString "/" <> fromMaybe mempty resourceFrom) ||
	   not (fromString "CHEOGRAM%" `T.isPrefixOf` fromMaybe mempty (messageID m))) then
		return [mkStanzaRec $ mkSMS componentJid smsJid txt]
	else do
		log "MESSAGE FROM WRONG GROUP" (fmap bareTxt existingRoom, bareFrom, m)
		return []
	where
	txt = mconcat [fromString "(", fromMaybe (fromString "nonick") resourceFrom, fromString ") ", body]
componentMessage db componentJid m@(Message { messageFrom = Just from, messageTo = Just to }) existingRoom bareFrom resourceFrom smsJid (Just body) = do
	ack <- case isNamed (fromString "{urn:xmpp:receipts}request") =<< messagePayloads m of
		(_:_) ->
			routeDiscoOrReply db componentJid from smsJid ("CHEOGRAM%query-then-send-ack%" ++ extra) Nothing
				(deliveryReceipt (fromMaybe mempty $ messageID m) to from)
		[] -> return []

	fmap (++ack) $ toRouteOrFallback db componentJid bareFrom resourceFrom smsJid strippedM $
		case PCRE.match autolinkRegex (encodeUtf8 body) [] of
			Just _ -> do
				log "WHISPER URL" m
				return [mkStanzaRec $ m {
					messageFrom = Just to,
					messageTo = Just from,
					messageType = MessageError,
					messagePayloads = messagePayloads m ++ [
						Element (fromString "{jabber:component:accept}error")
						[(fromString "{jabber:component:accept}type", [ContentText $ fromString "auth"])]
						[NodeElement $ Element (fromString "{urn:ietf:params:xml:ns:xmpp-stanzas}forbidden") [] []]
					]
				}]
			Nothing -> do
				nick <- nickFor db from existingRoom
				let txt = mconcat [fromString "(", nick, fromString " whispers) ", strippedBody]
				return [mkStanzaRec $ mkSMS componentJid smsJid txt]
	where
	strippedM = mapBody (const strippedBody) m
	strippedBody = stripOtrWhitespace body
	extra = T.unpack $ escapeJid $ T.pack $ show (fromMaybe mempty (messageID m), fromMaybe mempty resourceFrom)
componentMessage _ _ m _ _ _ _ _ = do
	log "UNKNOWN MESSAGE" m
	return []

handleJoinPartRoom db toRoomPresences toRejoinManager toJoinPartDebouncer componentJid existingRoom from to smsJid payloads join
	| join,
	  [x] <- isNamed (fromString "{http://jabber.org/protocol/muc#user}x") =<< payloads,
	  not $ null $ code "110" =<< isNamed (fromString "{http://jabber.org/protocol/muc#user}status") =<< elementChildren x = do
		existingInvite <- tcGetJID db to "invited"
		when (existingInvite == parseJID bareMUC) $ do
			let Just invitedKey = tcKey to "invited"
			True <- TC.runTCM $ TC.out db invitedKey
			return ()
		tcPutJID db to "joined" from
		let Just bookmarksKey = tcKey to "bookmarks"
		bookmarks <- fmap (fromMaybe [] . (readZ =<<)) (TC.runTCM $ TC.get db bookmarksKey)
		tcPut db to "bookmarks" (show $ sort $ nub $ T.unpack bareMUC : bookmarks)

		presences <- syncCall toRoomPresences $ GetRoomPresences to from
		atomically $ writeTChan toRoomPresences $ RecordSelfJoin to from (Just to)

		atomically $ writeTChan toRejoinManager $ Joined from

		case presences of
			[] -> do -- No one in the room, so we "created"
				uuid <- fromMaybe "UUIDFAIL" <$> (fmap.fmap) (fromString . UUID.toString) UUID.nextUUID
				let fullid = if (T.unpack resourceFrom `elem` map fst presences) then uuid else "CHEOGRAMCREATE%" <> uuid
				return [mkStanzaRec $ (emptyIQ IQGet) {
					iqTo = Just room,
					iqFrom = Just to,
					iqID = Just $ fromString fullid,
					iqPayload = Just $ Element (fromString "{http://jabber.org/protocol/muc#owner}query") [] []
				}]
			(_:_) | isNothing (lookup (T.unpack resourceFrom) presences) -> do
				fmap ((mkStanzaRec $ mkSMS componentJid smsJid $ mconcat [
						fromString "* You have joined ", bareMUC,
						fromString " as ", resourceFrom,
						fromString " along with\n",
						fromString $ intercalate ", " (filter (/= T.unpack resourceFrom) $ map fst presences)
					]):)
					(queryDisco room to)
			_ -> do
				log "JOINED" (to, from, "FALSE PRESENCE")
				queryDisco room to
	| not join,
	  [x] <- isNamed (fromString "{http://jabber.org/protocol/muc#user}x") =<< payloads,
	  (_:_) <- code "303" =<< isNamed (fromString "{http://jabber.org/protocol/muc#user}status") =<< elementChildren x = do
		let mnick = attributeText (fromString "nick") =<<
			listToMaybe (isNamed (fromString "{http://jabber.org/protocol/muc#user}item") =<< elementChildren x)
		toList <$> forM mnick (\nick -> do
			atomically $ writeTChan toRoomPresences $ RecordNickChanged to from nick
			return $ mkStanzaRec $ mkSMS componentJid smsJid $ mconcat [
					fromString "* ",
					resourceFrom,
					fromString " has changed their nick to ",
					nick
				]
			)
	| not join,
	  [x] <- isNamed (fromString "{http://jabber.org/protocol/muc#user}x") =<< payloads,
	  (_:_) <- code "332" =<< isNamed (fromString "{http://jabber.org/protocol/muc#user}status") =<< elementChildren x = do
		log "SERVER RESTART, rejoin in 5s" (to, from)
		void $ forkIO $ threadDelay 5000000 >> atomically (writeTChan toRejoinManager $ ForceRejoin from to)
		return []
	| not join && existingRoom == Just from = do
		let Just joinedKey = tcKey to "joined"
		True <- TC.runTCM $ TC.out db joinedKey
		atomically $ writeTChan toRoomPresences $ RecordPart to from
		atomically $ writeTChan toRoomPresences $ Clear to from
		return [mkStanzaRec $ mkSMS componentJid smsJid (fromString "* You have left " <> bareMUC)]
	| fmap bareTxt existingRoom == Just bareMUC && join = do
		atomically $ writeTChan toJoinPartDebouncer $ DebounceJoin to from (participantJid payloads)
		return []
	| fmap bareTxt existingRoom == Just bareMUC && not join = do
		atomically $ writeTChan toJoinPartDebouncer $ DebouncePart to from
		return []
	| join,
	  (_:_) <- isNamed (fromString "{http://jabber.org/protocol/muc#user}x") =<< payloads = do
		log "UNKNOWN JOIN" (existingRoom, from, to, payloads, join)
		atomically $ writeTChan toRoomPresences $ RecordJoin to from (participantJid payloads)
		return []
	| (_:_) <- isNamed (fromString "{http://jabber.org/protocol/muc#user}x") =<< payloads = do
		log "UNKNOWN NOT JOIN" (existingRoom, from, to, payloads, join)
		atomically $ writeTChan toRoomPresences $ RecordPart to from
		return []
	| otherwise =
		-- This is just presence. It's not marked as MUC or from the room this user is in
		return []
	where
	resourceFrom = fromMaybe mempty (strResource <$> jidResource from)
	Just room = parseJID bareMUC
	bareMUC = bareTxt from

verificationResponse =
	Element (fromString "{jabber:iq:register}query") []
		[
			NodeElement $ Element (fromString "{jabber:iq:register}instructions") [] [
				NodeContent $ ContentText $ fromString "Enter the verification code CheoGram texted you."
			],
			NodeElement $ Element (fromString "{jabber:iq:register}password") [] [],
			NodeElement $ Element (fromString "{jabber:x:data}x") [
				(fromString "{jabber:x:data}type", [ContentText $ fromString "form"])
			] [
				NodeElement $ Element (fromString "{jabber:x:data}title") [] [NodeContent $ ContentText $ fromString "Verify Phone Number"],
				NodeElement $ Element (fromString "{jabber:x:data}instructions") [] [
					NodeContent $ ContentText $ fromString "Enter the verification code CheoGram texted you."
				],
				NodeElement $ Element (fromString "{jabber:x:data}field") [
					(fromString "{jabber:x:data}type", [ContentText $ fromString "hidden"]),
					(fromString "{jabber:x:data}var", [ContentText $ fromString "FORM_TYPE"])
				] [
					NodeElement $ Element (fromString "{jabber:x:data}value") [] [NodeContent $ ContentText $ fromString "jabber:iq:register"]
				],
				NodeElement $ Element (fromString "{jabber:x:data}field") [
					(fromString "{jabber:x:data}type", [ContentText $ fromString "text-single"]),
					(fromString "{jabber:x:data}var", [ContentText $ fromString "password"]),
					(fromString "{jabber:x:data}label", [ContentText $ fromString "Verification code"])
				] []
			]
		]

data RegistrationCode = RegistrationCode { regCode :: Int, cheoJid :: Text, expires :: UTCTime } deriving (Show, Read)

registerVerification db componentJid to iq = do
	code <- getStdRandom (randomR (123457::Int,987653))
	time <- getCurrentTime
	True <- TC.runTCM $ TC.put db ((maybe mempty T.unpack $ bareTxt <$> iqFrom iq) <> "\0registration_code") $ show $ RegistrationCode code (formatJID to) time
	return [
			mkStanzaRec $ mkSMS componentJid to $ fromString ("Enter this verification code to complete registration: " <> show code),
			mkStanzaRec $ iq {
				iqTo = iqFrom iq,
				iqFrom = iqTo iq,
				iqType = IQResult,
				iqPayload = Just verificationResponse
			}
		]

handleVerificationCode db componentJid password iq = do
	time <- getCurrentTime
	codeAndTime <- fmap (readZ =<<) $ TC.runTCM $ TC.get db regKey
	case codeAndTime of
		Just (RegistrationCode { regCode = code, cheoJid = cheoJidT })
			| fmap expires codeAndTime > Just ((-300) `addUTCTime` time) ->
				case (show code == T.unpack password, iqTo iq, iqFrom iq, parseJID cheoJidT) of
					(True, Just to, Just from, Just cheoJid) -> do
						bookmarks <- fmap (fromMaybe [] . (readZ =<<)) (maybe (return Nothing) (TC.runTCM . TC.get db) (tcKey cheoJid "bookmarks"))
						invites <- fmap concat $ forM (mapMaybe parseJID bookmarks) $ \bookmark ->
							sendInvite db from (Invite bookmark cheoJid (Just $ fromString "Cheogram registration") Nothing)

						let Just tel = T.unpack . strNode <$> jidNode cheoJid
						True <- TC.runTCM $ TC.put db (T.unpack (bareTxt from) <> "\0registered") tel
						tcPutJID db cheoJid "registered" from

						stuff <- runMaybeT $ do
							-- If there is a nick that doesn't end in _sms, add _sms
							nick <- MaybeT . TC.runTCM . TC.get db =<< (hoistMaybe $ tcKey cheoJid "nick")
							let nick' = (fromMaybe (fromString nick) $ T.stripSuffix (fromString "_sms") (fromString nick)) <> fromString "_sms"
							tcPut db cheoJid "nick" (T.unpack nick')

							room <- MaybeT ((parseJID <=< fmap bareTxt) <$> tcGetJID db cheoJid "joined")
							toJoin <- hoistMaybe $ parseJID (bareTxt room <> fromString "/" <> nick')
							liftIO $ joinRoom db cheoJid toJoin

						return ((mkStanzaRec $ iq {
								iqTo = iqFrom iq,
								iqFrom = iqTo iq,
								iqType = IQResult,
								iqPayload = Just $ Element (fromString "{jabber:iq:register}query") [] []
							}):invites)
					_ ->
						return [mkStanzaRec $ iq {
							iqTo = iqFrom iq,
							iqFrom = iqTo iq,
							iqType = IQError,
							iqPayload = Just $ Element (fromString "{jabber:component:accept}error")
								[(fromString "{jabber:component:accept}type", [ContentText $ fromString "auth"])]
								[NodeElement $ Element (fromString "{urn:ietf:params:xml:ns:xmpp-stanzas}not-authorized") [] []]
						}]
		_ -> do
			void $ TC.runTCM $ TC.out db regKey
			return []
	where
	regKey = (maybe mempty T.unpack $ bareTxt <$> iqFrom iq) <> "\0registration_code"

handleRegister db componentJid iq@(IQ { iqType = IQGet }) _ = do
	time <- getCurrentTime
	codeAndTime <- fmap (readZ =<<) $ TC.runTCM $ TC.get db ((maybe mempty T.unpack $ bareTxt <$> iqFrom iq) <> "\0registration_code")
	if fmap expires codeAndTime > Just ((-300) `addUTCTime` time) then
		return [mkStanzaRec $ iq {
			iqTo = iqFrom iq,
			iqFrom = iqTo iq,
			iqType = IQResult,
			iqPayload = Just verificationResponse
		}]
	else
		return [mkStanzaRec $ iq {
			iqTo = iqFrom iq,
			iqFrom = iqTo iq,
			iqType = IQResult,
			iqPayload = Just $ Element (fromString "{jabber:iq:register}query") []
				[
					NodeElement $ Element (fromString "{jabber:iq:register}instructions") [] [
						NodeContent $ ContentText $ fromString "CheoGram can verify your phone number and add you to the private groups you previously texted."
					],
					NodeElement $ Element (fromString "{jabber:iq:register}phone") [] [],
					NodeElement $ Element (fromString "{jabber:x:data}x") [
						(fromString "{jabber:x:data}type", [ContentText $ fromString "form"])
					] [
						NodeElement $ Element (fromString "{jabber:x:data}title") [] [NodeContent $ ContentText $ fromString "Associate Phone Number"],
						NodeElement $ Element (fromString "{jabber:x:data}instructions") [] [
							NodeContent $ ContentText $ fromString "CheoGram can verify your phone number and add you to the private groups you previously texted."
						],
						NodeElement $ Element (fromString "{jabber:x:data}field") [
							(fromString "{jabber:x:data}type", [ContentText $ fromString "hidden"]),
							(fromString "{jabber:x:data}var", [ContentText $ fromString "FORM_TYPE"])
						] [
							NodeElement $ Element (fromString "{jabber:x:data}value") [] [NodeContent $ ContentText $ fromString "jabber:iq:register"]
						],
						NodeElement $ Element (fromString "{jabber:x:data}field") [
							(fromString "{jabber:x:data}type", [ContentText $ fromString "text-single"]),
							(fromString "{jabber:x:data}var", [ContentText $ fromString "phone"]),
							(fromString "{jabber:x:data}label", [ContentText $ fromString "Phone number"])
						] []
					]
				]
		}]
handleRegister db componentJid iq@(IQ { iqType = IQSet }) query
	| [form] <- isNamed (fromString "{jabber:x:data}x") =<< elementChildren query,
	  Just to <- ((`telToJid` formatJID componentJid) . T.filter isDigit) =<< getFormField form (fromString "phone") = do
		registerVerification db componentJid to iq
handleRegister db componentJid iq@(IQ { iqType = IQSet }) query
	| [phoneEl] <- isNamed (fromString "{jabber:iq:register}phone") =<< elementChildren query,
	  Just to <- (`telToJid` formatJID componentJid) $ T.filter isDigit $ mconcat (elementText phoneEl) = do
		registerVerification db componentJid to iq
handleRegister db componentJid iq@(IQ { iqType = IQSet }) query
	| [form] <- isNamed (fromString "{jabber:x:data}x") =<< elementChildren query,
	  Just password <- getFormField form (fromString "password") = do
		handleVerificationCode db componentJid password iq
handleRegister db componentJid iq@(IQ { iqType = IQSet, iqPayload = Just payload }) query
	| [passwordEl] <- isNamed (fromString "{jabber:iq:register}password") =<< elementChildren query = do
		handleVerificationCode db componentJid (mconcat $ elementText passwordEl) iq
handleRegister db componentJid iq@(IQ { iqType = IQSet }) query
	| [_] <- isNamed (fromString "{jabber:iq:register}remove") =<< elementChildren query = do
		tel <- maybe mempty T.pack <$> TC.runTCM (TC.get db $ T.unpack (maybe mempty bareTxt $ iqFrom iq) <> "\0registered")
		forM_ (telToJid tel (formatJID componentJid) >>= \cheoJid -> tcKey cheoJid "registered") $ \regKey ->
			TC.runTCM $ TC.out db regKey
		void $ TC.runTCM $ TC.out db $ T.unpack (maybe mempty bareTxt $ iqFrom iq) <> "\0registered"
		return [mkStanzaRec $ iq {
			iqTo = iqFrom iq,
			iqFrom = iqTo iq,
			iqType = IQResult,
			iqPayload = Just $ Element (fromString "{jabber:iq:register}query") [] []
		}]
handleRegister _ _ iq@(IQ { iqType = typ }) _
	| typ `elem` [IQGet, IQSet] = do
		log "HANDLEREGISTER return error" iq
		return [mkStanzaRec $ iq {
			iqTo = iqFrom iq,
			iqFrom = iqTo iq,
			iqType = IQError,
			iqPayload = Just $ Element (fromString "{jabber:component:accept}error")
				[(fromString "{jabber:component:accept}type", [ContentText $ fromString "cancel"])]
				[NodeElement $ Element (fromString "{urn:ietf:params:xml:ns:xmpp-stanzas}feature-not-implemented") [] []]
		}]
handleRegister _ _ iq _ = do
	log "HANDLEREGISTER UNKNOWN" iq
	return []

data ComponentContext = ComponentContext {
	db :: TC.HDB,
	smsJid :: Maybe JID,
	registrationJids :: [JID],
	adhocBotMessage :: Message -> STM (),
	ctxCacheOOB :: Message -> UIO Message,
	toRoomPresences :: TChan RoomPresences,
	toRejoinManager :: TChan RejoinManagerCommand,
	toJoinPartDebouncer :: TChan JoinPartDebounce,
	processDirectMessageRouteConfig :: IQ -> IO (Maybe IQ),
	componentJid :: JID,
	sendIQ :: IQ -> UIO (STM (Maybe IQ)),
	maybeAvatar :: Maybe Avatar
}

componentStanza :: ComponentContext -> ReceivedStanza -> IO [StanzaRec]
componentStanza (ComponentContext { adhocBotMessage, ctxCacheOOB, componentJid }) (ReceivedMessage (m@Message { messageTo = Just (JID { jidNode = Nothing }) }))
	| Just reply <- groupTextPorcelein (formatJID componentJid) m = do
		-- TODO: only when from direct message route
		-- TODO: only if target does not understand stanza addressing
		reply' <- UIO.lift $ ctxCacheOOB reply
		return [mkStanzaRec reply']
	| Just _ <- getBody "jabber:component:accept" m = do
		atomicUIO $ adhocBotMessage m
		return []
	| otherwise = log "WEIRD BODYLESS MESSAGE DIRECT TO COMPONENT" m >> return []
componentStanza (ComponentContext { db, componentJid, sendIQ, smsJid = Just smsJid }) (ReceivedMessage (m@Message { messageTo = Just to, messageFrom = Just from}))
	| [propose] <- isNamed (fromString "{urn:xmpp:jingle-message:0}propose") =<< messagePayloads m = do
		let sid = fromMaybe mempty $ XML.attributeText (s"id") propose
		telFeatures <- getTelFeatures db componentJid sendIQ from
		stanzas <- routeDiscoOrReply db componentJid from smsJid "CHEOGRAM%query-then-send-presence" Nothing $ telAvailable to from telFeatures
		return $ (mkStanzaRec $ (XMPP.emptyMessage XMPP.MessageNormal) {
				XMPP.messageID = Just $ s"proceed%" ++ sid,
				XMPP.messageTo = Just from,
				XMPP.messageFrom = XMPP.parseJID $ bareTxt to ++ s"/tel",
				XMPP.messagePayloads = [
					XML.Element (s"{urn:xmpp:jingle-message:0}proceed")
						[(s"id", [XML.ContentText sid])] []
				]
			}) : stanzas
componentStanza _ (ReceivedMessage (m@Message { messageTo = Just to, messageFrom = Just from}))
	| [x] <- isNamed (fromString "{http://jabber.org/protocol/muc#user}x") =<< messagePayloads m,
	  not $ null $ code "104" =<< isNamed (fromString "{http://jabber.org/protocol/muc#user}status") =<< elementChildren x = do
		queryDisco from to
componentStanza (ComponentContext { db, smsJid = (Just smsJid), componentJid }) (ReceivedMessage (m@Message { messageTo = Just to, messageFrom = Just from})) = do
	existingRoom <- tcGetJID db to "joined"
	componentMessage db componentJid m existingRoom (bareTxt from) resourceFrom smsJid $
		getBody "jabber:component:accept" m
	where
	resourceFrom = strResource <$> jidResource from
componentStanza (ComponentContext { smsJid = (Just smsJid), toRejoinManager, componentJid }) (ReceivedPresence p@(Presence { presenceType = PresenceError, presenceFrom = Just from, presenceTo = Just to, presenceID = Just id }))
	| fromString "CHEOGRAMREJOIN%" `T.isPrefixOf` id = do
		log "FAILED TO REJOIN, try again in 10s" p
		void $ forkIO $ threadDelay 10000000 >> atomically (writeTChan toRejoinManager $ ForceRejoin from to)
		return []
	| fromString "CHEOGRAMJOIN%" `T.isPrefixOf` id = do
		log "FAILED TO JOIN" p
		let errorText = maybe mempty (mconcat . (fromString "\n":) . elementText) $ listToMaybe $
			isNamed (fromString "{urn:ietf:params:xml:ns:xmpp-stanzas}text") =<<
			elementChildren =<< isNamed (fromString "{jabber:component:accept}error") =<< presencePayloads p
		return [mkStanzaRec $ mkSMS componentJid smsJid (fromString "* Failed to join " <> bareTxt from <> errorText)]
	| otherwise = return [] -- presence error from a non-MUC, just ignore
componentStanza (ComponentContext { db, smsJid = (Just smsJid), toRoomPresences, toRejoinManager, toJoinPartDebouncer, componentJid }) (ReceivedPresence (Presence {
		presenceType = typ,
		presenceFrom = Just from,
		presenceTo = Just to,
		presencePayloads = payloads
	})) | typ `elem` [PresenceAvailable, PresenceUnavailable] = do
		existingRoom <- tcGetJID db to "joined"
		handleJoinPartRoom db toRoomPresences toRejoinManager toJoinPartDebouncer componentJid existingRoom from to smsJid payloads (typ == PresenceAvailable)
componentStanza (ComponentContext { db, componentJid, sendIQ, maybeAvatar }) (ReceivedPresence (Presence { presenceType = PresenceSubscribe, presenceFrom = Just from, presenceTo = Just to@JID { jidNode = Nothing } })) = do
	avail <- cheogramAvailable db componentJid sendIQ to from
	return $ [
			mkStanzaRec $ (emptyPresence PresenceSubscribed) {
				presenceTo = Just from,
				presenceFrom = Just to
			},
			mkStanzaRec $ (emptyPresence PresenceSubscribe) {
				presenceTo = Just from,
				presenceFrom = Just to
			},
			mkStanzaRec avail
		] ++ map (mkStanzaRec . (\payload -> ((emptyMessage MessageHeadline) {
			messageTo = Just from,
			messageFrom = Just to,
			messagePayloads = [payload]
		})) . avatarMetadata) (justZ maybeAvatar)
componentStanza (ComponentContext { db, smsJid = (Just smsJid), componentJid }) (ReceivedPresence (Presence { presenceType = PresenceSubscribe, presenceFrom = Just from, presenceTo = Just to@JID { jidNode = Just _ } })) = do
	stanzas <- routeDiscoOrReply db componentJid from smsJid "CHEOGRAM%query-then-send-presence" Nothing $ telAvailable to from []
	return $ [
			mkStanzaRec $ (emptyPresence PresenceSubscribed) {
				presenceTo = Just from,
				presenceFrom = Just to
			},
			mkStanzaRec $ (emptyPresence PresenceSubscribe) {
				presenceTo = Just from,
				presenceFrom = Just to
			}
		] ++ stanzas
componentStanza (ComponentContext { smsJid = Nothing }) (ReceivedPresence (Presence { presenceType = PresenceSubscribe, presenceFrom = Just from, presenceTo = Just to@JID { jidNode = Just node } }))
	| Just _ <- mapM localpartToURI (T.split (==',') $ strNode node) = do
	return $ [
			mkStanzaRec $ (emptyPresence PresenceSubscribed) {
				presenceTo = Just from,
				presenceFrom = Just to
			},
			mkStanzaRec $ (emptyPresence PresenceSubscribe) {
				presenceTo = Just from,
				presenceFrom = Just to
			},
			mkStanzaRec $ telAvailable to from []
		]
componentStanza (ComponentContext { db, componentJid, sendIQ, maybeAvatar }) (ReceivedPresence (Presence { presenceType = PresenceProbe, presenceFrom = Just from, presenceTo = Just to@JID { jidNode = Nothing } })) = do
	avail <- cheogramAvailable db componentJid sendIQ to from
	return $
		[mkStanzaRec avail] ++
		map (mkStanzaRec . (\payload -> (emptyMessage (MessageHeadline)) {
			messageTo = Just from,
			messageFrom = Just to,
			messagePayloads = [payload]
		}) . avatarMetadata) (justZ maybeAvatar)
componentStanza (ComponentContext { db, smsJid = (Just smsJid), componentJid }) (ReceivedPresence (Presence { presenceType = PresenceProbe, presenceFrom = Just from, presenceTo = Just to@JID { jidNode = Just _ } })) = do
	routeDiscoOrReply db componentJid from smsJid "CHEOGRAM%query-then-send-presence" Nothing $ telAvailable to from []
componentStanza _ (ReceivedPresence (Presence { presenceType = PresenceProbe, presenceFrom = Just from, presenceTo = Just to@JID { jidNode = Just node } }))
	| Just multipleTo <- mapM localpartToURI (T.split (==',') $ strNode node) = do
	return $ [mkStanzaRec $ telAvailable to from []]
componentStanza (ComponentContext { maybeAvatar = Just (Avatar hash _ b64) }) (ReceivedIQ (iq@IQ { iqType = IQGet, iqTo = Just to@JID { jidNode = Nothing }, iqFrom = Just from, iqID = Just id, iqPayload = Just p }))
	| [items] <- isNamed (s"{http://jabber.org/protocol/pubsub}items") =<<
		elementChildren =<<
		isNamed (s"{http://jabber.org/protocol/pubsub}pubsub") p,
	  attributeText (s"node") items == Just (s"urn:xmpp:avatar:data"),
	  [item] <- isNamed (s"{http://jabber.org/protocol/pubsub}item") =<<
		elementChildren items,
	  attributeText (s"id") item == Just hash =
		return [mkStanzaRec $ iqReply (Just $
			XML.Element (s"{http://jabber.org/protocol/pubsub}pubsub") [] [
				XML.NodeElement $ XML.Element (s"{http://jabber.org/protocol/pubsub}items")
				[(s"node", [XML.ContentText $ s"urn:xmpp:avatar:data"])] [
					XML.NodeElement $ XML.Element (s"{http://jabber.org/protocol/pubsub}item")
					[(s"id", [XML.ContentText hash])] [
						XML.NodeElement $ mkElement (s"{urn:xmpp:avatar:data}data") b64
					]
				]
			]
		) iq]
componentStanza (ComponentContext { registrationJids, processDirectMessageRouteConfig, componentJid }) (ReceivedIQ (IQ { iqType = IQSet, iqTo = Just to, iqFrom = Just from, iqID = Just id, iqPayload = Just p }))
	| jidNode to == Nothing,
	  [iqEl] <- isNamed (s"{jabber:client}iq") =<< elementChildren =<< isNamed (s"{urn:xmpp:forward:0}forwarded") p,
	  [payload] <- isNamed (s"{http://jabber.org/protocol/commands}command") =<< elementChildren iqEl,
	  Just asFrom <- parseJID =<< attributeText (s"from") iqEl,
	  bareTxt from `elem` map bareTxt registrationJids = do
		replyIQ <- processDirectMessageRouteConfig $ (emptyIQ IQSet) {
				iqID = Just id,
				iqTo = Just to,
				iqFrom = Just asFrom,
				iqPayload = Just payload
			}
		fmap (fromMaybe []) $ forM replyIQ $ \replyIQ -> do
		--(\f -> maybe (return []) f replyIQ) $ \replyIQ -> do
			let fromLocalpart = maybe mempty (\localpart -> localpart++s"@") (fmap strNode . jidNode =<< iqFrom replyIQ)

			let subscribe = if attributeText (s"action") payload /= Just (s"complete") then [] else [
					mkStanzaRec $ (emptyPresence PresenceSubscribe) {
						presenceTo = Just asFrom,
						presenceFrom = Just componentJid,
						presencePayloads = [
							Element (s"{jabber:component:accept}status") [] [
								NodeContent $ ContentText $ s"Add this contact and then you can SMS by sending messages to +1<phone-number>@" ++ formatJID componentJid ++ s" Jabber IDs."
							]
						]
					}
				]

			return $ subscribe ++ [mkStanzaRec $ replyIQ {
				iqTo = if iqTo replyIQ == Just asFrom then Just from else iqTo replyIQ,
				iqID = if iqType replyIQ == IQResult then iqID replyIQ else Just $ fromString $ show (formatJID from, formatJID asFrom, iqID replyIQ),
				iqFrom = parseJID (fromLocalpart ++ formatJID componentJid ++ s"/CHEOGRAM%" ++ ConfigureDirectMessageRoute.nodeName)
			}]
componentStanza (ComponentContext { processDirectMessageRouteConfig, componentJid }) (ReceivedIQ iq@(IQ { iqTo = Just to }))
	| fmap strResource (jidResource to) == Just (s"CHEOGRAM%" ++ ConfigureDirectMessageRoute.nodeName),
	  Just (fwdBy, onBehalf, iqId) <- readZ . T.unpack =<< iqID iq = do
		replyIQ <- processDirectMessageRouteConfig (iq { iqID = iqId })
		fmap (fromMaybe []) $ forM replyIQ $ \replyIQ -> do
			let fromLocalpart = maybe mempty (\localpart -> localpart++s"@") (fmap strNode . jidNode =<< iqFrom replyIQ)
			return [mkStanzaRec $ replyIQ {
				iqTo = if fmap bareTxt (iqTo replyIQ) == Just onBehalf then parseJID fwdBy else iqTo replyIQ,
				iqID = if iqType replyIQ == IQResult then iqID replyIQ else Just $ fromString $ show (fwdBy, onBehalf, iqID replyIQ),
				iqFrom = parseJID (fromLocalpart ++ formatJID componentJid ++ s"/CHEOGRAM%" ++ ConfigureDirectMessageRoute.nodeName)
			}]
componentStanza (ComponentContext { processDirectMessageRouteConfig, componentJid }) (ReceivedIQ iq@(IQ { iqTo = Just to, iqPayload = payload }))
	| (jidNode to == Nothing && fmap elementName payload == Just (s"{http://jabber.org/protocol/commands}command") && (attributeText (s"node") =<< payload) == Just ConfigureDirectMessageRoute.nodeName) ||
	  fmap strResource (jidResource to) == Just (s"CHEOGRAM%" ++ ConfigureDirectMessageRoute.nodeName) = do
		replyIQ <- processDirectMessageRouteConfig iq
		fmap (fromMaybe []) $ forM replyIQ $ \replyIQ -> do
			let fromLocalpart = maybe mempty (\localpart -> localpart++s"@") (fmap strNode . jidNode =<< iqFrom replyIQ)
			return [mkStanzaRec $ replyIQ {
				iqFrom = parseJID (fromLocalpart ++ formatJID componentJid ++ s"/CHEOGRAM%" ++ ConfigureDirectMessageRoute.nodeName)
			}]
componentStanza (ComponentContext { db, componentJid }) (ReceivedIQ iq@(IQ { iqTo = Just to, iqPayload = Just payload, iqFrom = Just from }))
	| jidNode to == Nothing,
	  elementName payload == s"{http://jabber.org/protocol/commands}command",
	  attributeText (s"node") payload == Just (s"sip-proxy-set"),
	  [form] <- isNamed (fromString "{jabber:x:data}x") =<< elementChildren payload,
	  Just proxy <- getFormField form (s"sip-proxy") = do
		True <- if T.null proxy then
			TC.runTCM $ TC.out db $ T.unpack (bareTxt from) ++ "\0sip-proxy"
		else
			TC.runTCM $ TC.put db (T.unpack (bareTxt from) ++ "\0sip-proxy") $ T.unpack proxy
		return [mkStanzaRec $ iqReply Nothing iq]
componentStanza (ComponentContext { db, componentJid }) (ReceivedIQ iq@(IQ { iqTo = Just to, iqPayload = Just payload, iqFrom = Just from }))
	| jidNode to == Nothing,
	  jidNode from == Nothing,
	  elementName payload == s"{http://jabber.org/protocol/commands}command",
	  attributeText (s"node") payload == Just (s"push-register"),
	  [form] <- isNamed (fromString "{jabber:x:data}x") =<< elementChildren payload,
	  Just pushRegisterTo <- XMPP.parseJID =<< getFormField form (s"to") = do
		TC.runTCM (TC.put db (T.unpack (bareTxt pushRegisterTo) ++ "\0possible-route") (T.unpack $ XMPP.formatJID from))
		return [
				mkStanzaRec $ iqReply (
					Just $ Element (s"{http://jabber.org/protocol/commands}command")
					[
						(s"{http://jabber.org/protocol/commands}node", [ContentText $ s"push-register"]),
						(s"{http://jabber.org/protocol/commands}sessionid", [ContentText $ s"all-done"]),
						(s"{http://jabber.org/protocol/commands}status", [ContentText $ s"completed"])
					]
					[
						NodeElement $ Element (fromString "{jabber:x:data}x") [
							(fromString "{jabber:x:data}type", [ContentText $ s"result"])
						] [
							NodeElement $ Element (fromString "{jabber:x:data}field") [
								(fromString "{jabber:x:data}type", [ContentText $ s"jid-single"]),
								(fromString "{jabber:x:data}var", [ContentText $ s"from"])
							] [
								NodeElement $ Element (fromString "{jabber:x:data}value") [] [NodeContent $ ContentText $ escapeJid (bareTxt pushRegisterTo) ++ s"@" ++ formatJID componentJid]
							]
						]
					]
				) iq,
				mkStanzaRec $ mkSMS componentJid pushRegisterTo $
					s"To start registration with " ++ XMPP.formatJID from ++ s" reply with: register " ++ XMPP.formatJID from ++
					s"\n(If you do not wish to start this registration, simply ignore this message.)"
			]
componentStanza _ (ReceivedIQ iq@(IQ { iqFrom = Just _, iqTo = Just (JID { jidNode = Nothing }), iqPayload = Just p }))
	| iqType iq `elem` [IQGet, IQSet],
	  [_] <- isNamed (fromString "{jabber:iq:register}query") p = do
		return [mkStanzaRec $ iqNotImplemented iq]
componentStanza (ComponentContext { db, componentJid, maybeAvatar, sendIQ }) (ReceivedIQ (IQ { iqType = IQGet, iqFrom = Just from, iqTo = Just to, iqID = id, iqPayload = Just p }))
	| Nothing <- jidNode to,
	  [q] <- isNamed (fromString "{http://jabber.org/protocol/disco#info}query") p = do
		payload <- cheogramDiscoInfo db componentJid sendIQ from (Just q)
		return [mkStanzaRec $ (emptyIQ IQResult) {
				iqTo = Just from,
				iqFrom = Just to,
				iqID = id,
				iqPayload = Just payload
			}]
	| Nothing <- jidNode to,
	  [s"http://jabber.org/protocol/commands"] ==
	    mapMaybe (attributeText (s"node")) (isNamed (fromString "{http://jabber.org/protocol/disco#items}query") p) = do
		routeQueryOrReply db componentJid from componentJid ("CHEOGRAM%query-then-send-command-list%" ++ extra) queryCommandList (commandList componentJid id to from [])
	| Nothing <- jidNode to,
	  [_] <- isNamed (s"{vcard-temp}vCard") p =
		return [mkStanzaRec $ (emptyIQ IQResult) {
			iqTo = Just from,
			iqFrom = Just to,
			iqID = id,
			iqPayload = Just $ Element (s"{vcard-temp}vCard") [] $
				[
					NodeElement $ Element (s"{vcard-temp}URL") [] [NodeContent $ ContentText $ s"https://cheogram.com"],
					NodeElement $ Element (s"{vcard-temp}DESC") [] [NodeContent $ ContentText $ s"Cheogram provides stable JIDs for PSTN identifiers, with routing through many possible backends.\n\n© Stephen Paul Weber, licensed under AGPLv3+.\n\nSource code for this gateway is available from the listed homepage.\n\nPart of the Soprani.ca project."]
				] ++ map (\(Avatar _ _ b64) -> NodeElement $ Element (s"{vcard-temp}PHOTO") [] [
						NodeElement $ mkElement (s"{vcard-temp}TYPE") (s"image/png"),
						NodeElement $ mkElement (s"{vcard-temp}BINVAL") b64
					]
				) (justZ maybeAvatar)
		}]
	where
	extra = T.unpack $ escapeJid $ T.pack $ show (id, fromMaybe mempty resourceFrom)
	resourceFrom = strResource <$> jidResource from
componentStanza (ComponentContext { db, sendIQ, smsJid = (Just smsJid), componentJid }) (ReceivedIQ (IQ { iqType = IQGet, iqFrom = Just from, iqTo = Just to, iqID = Just id, iqPayload = Just p }))
	| Just _ <- jidNode to,
	  [q] <- isNamed (fromString "{http://jabber.org/protocol/disco#info}query") p = do
		maybeDiscoResult <- routeDiscoStateful db componentJid sendIQ from (jidNode smsJid) (nodeAttribute q)
		telFeatures <- getTelFeatures db componentJid sendIQ from
		case maybeDiscoResult of
			Just (IQ { iqPayload = Just discoResult }) -> return [
					mkStanzaRec $ telDiscoInfo q id to from $ (telFeatures ++) $ mapMaybe (attributeText (fromString "var")) $
					isNamed (fromString "{http://jabber.org/protocol/disco#info}feature") =<< elementChildren discoResult
				]
			Nothing -> return [mkStanzaRec $ telDiscoInfo q id to from telFeatures]
	| Just tel <- strNode <$> jidNode to,
	  [_] <- isNamed (s"{vcard-temp}vCard") p = do
		--owners <- (fromMaybe [] . (readZ =<<)) <$>
		--	maybe (return Nothing) (TC.runTCM . TC.get db) (tcKey smsJid "owners")
		return [mkStanzaRec $ (emptyIQ IQResult) {
			iqTo = Just from,
			iqFrom = Just to,
			iqID = Just id,
			iqPayload = Just $ Element (s"{vcard-temp}vCard") [] (
					[
						NodeElement $ Element (s"{vcard-temp}TEL") [] [
							NodeElement $ Element (s"{vcard-temp}NUMBER") [] [NodeContent $ ContentText tel]
						]
					]
					--map (\owner -> NodeElement (Element (s"{vcard-temp}JABBERID") [] [NodeContent $ ContentText owner])) owners
				)
		}]
	where
	extra = T.unpack $ escapeJid $ T.pack $ show (id, fromMaybe mempty resourceFrom)
	resourceFrom = strResource <$> jidResource from
componentStanza (ComponentContext { componentJid }) (ReceivedIQ (iq@IQ { iqType = IQSet, iqFrom = Just from, iqTo = Just (to@JID {jidNode = Nothing}), iqID = id, iqPayload = Just p }))
	| [query] <- isNamed (fromString "{jabber:iq:gateway}query") p,
	  [prompt] <- isNamed (fromString "{jabber:iq:gateway}prompt") =<< elementChildren query = do
		case telToJid (T.filter isDigit $ mconcat $ elementText prompt) (formatJID componentJid) of
			Just jid ->
				return [mkStanzaRec $ (emptyIQ IQResult) {
					iqTo = Just from,
					iqFrom = Just to,
					iqID = id,
					iqPayload = Just $ Element (fromString "{jabber:iq:gateway}query") []
						[NodeElement $ Element (fromString "{jabber:iq:gateway}jid") [ ] [NodeContent $ ContentText $ formatJID jid]]
				}]
			Nothing ->
				return [mkStanzaRec $ iq {
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
				}]
componentStanza _ (ReceivedIQ (IQ { iqType = IQGet, iqFrom = Just from, iqTo = Just (to@JID {jidNode = Nothing}), iqID = id, iqPayload = Just p }))
	| [_] <- isNamed (fromString "{jabber:iq:gateway}query") p = do
		return [mkStanzaRec $ (emptyIQ IQResult) {
			iqTo = Just from,
			iqFrom = Just to,
			iqID = id,
			iqPayload = Just $ Element (fromString "{jabber:iq:gateway}query") []
				[
					NodeElement $ Element (fromString "{jabber:iq:gateway}desc") [ ] [NodeContent $ ContentText $ fromString "Please enter your contact's phone number"],
					NodeElement $ Element (fromString "{jabber:iq:gateway}prompt") [ ] [NodeContent $ ContentText $ fromString "Phone Number"]
				]
		}]
componentStanza (ComponentContext { db }) (ReceivedIQ (iq@IQ { iqType = IQError, iqFrom = Just from, iqTo = Just to }))
	| (strNode <$> jidNode to) == Just (fromString "create"),
	  Just resource <- strResource <$> jidResource to = do
		log "create@ ERROR" (from, to, iq)
		case T.splitOn (fromString "|") resource of
			(cheoJidT:_) | Just cheoJid <- parseJID cheoJidT -> do
				mnick <- maybe (return Nothing) (TC.runTCM . TC.get db) (tcKey cheoJid "nick")
				let nick = maybe (maybe mempty strNode (jidNode cheoJid)) fromString mnick
				let Just room = parseJID $ bareTxt from <> fromString "/" <> nick
				(++) <$>
					leaveRoom db cheoJid "Joined a different room." <*>
					joinRoom db cheoJid room
			_ -> return [] -- Invalid packet, ignore
componentStanza (ComponentContext { componentJid }) (ReceivedIQ (IQ { iqType = IQResult, iqFrom = Just from, iqTo = Just to }))
	| (strNode <$> jidNode to) == Just (fromString "create"),
	  Just resource <- strResource <$> jidResource to = do
		case T.splitOn (fromString "|") resource of
			(cheoJidT:name:[]) | Just cheoJid <- parseJID cheoJidT, Just tel <- strNode <$> jidNode cheoJid ->
				createRoom componentJid [strDomain $ jidDomain from] cheoJid (name <> fromString "_" <> tel)
			(cheoJidT:name:servers) | Just cheoJid <- parseJID cheoJidT ->
				createRoom componentJid servers cheoJid name
			_ -> return [] -- Invalid packet, ignore
componentStanza (ComponentContext { toRejoinManager }) (ReceivedIQ (IQ { iqType = IQResult, iqID = Just id, iqFrom = Just from }))
	| fromString "CHEOGRAMPING%" `T.isPrefixOf` id = do
		atomically $ writeTChan toRejoinManager (PingReply from)
		return []
componentStanza (ComponentContext { toRejoinManager }) (ReceivedIQ (IQ { iqType = IQError, iqID = Just id, iqFrom = Just from }))
	| fromString "CHEOGRAMPING%" `T.isPrefixOf` id = do
		atomically $ writeTChan toRejoinManager (PingError from)
		return []
componentStanza _ (ReceivedIQ (IQ { iqType = IQResult, iqFrom = Just from, iqTo = Just to, iqID = Just id, iqPayload = Just p }))
	| [query] <- isNamed (fromString "{http://jabber.org/protocol/muc#owner}query") p,
	  [form] <- isNamed (fromString "{jabber:x:data}x") =<< elementChildren query = do
		uuid <- fromMaybe "UUIDFAIL" <$> (fmap.fmap) (fromString . UUID.toString) UUID.nextUUID
		let fullid = if fromString "CHEOGRAMCREATE%" `T.isPrefixOf` id then "CHEOGRAMCREATE%" <> uuid else uuid
		return [mkStanzaRec $ (emptyIQ IQSet) {
			iqTo = Just from,
			iqFrom = Just to,
			iqID = Just $ fromString fullid,
			iqPayload = Just $ Element (fromString "{http://jabber.org/protocol/muc#owner}query") [] [
				NodeElement $
				fillFormField (fromString "muc#roomconfig_publicroom") (fromString "0") $
				fillFormField (fromString "muc#roomconfig_persistentroom") (fromString "1") $
				fillFormField (fromString "muc#roomconfig_allowinvites") (fromString "1") $
				fillFormField (fromString "muc#roomconfig_membersonly") (fromString "1")
				form { elementAttributes = [(fromString "{jabber:x:data}type", [ContentText $ fromString "submit"])] }
			]
		}]
componentStanza (ComponentContext { smsJid = (Just smsJid), componentJid }) (ReceivedIQ (IQ { iqType = IQResult, iqFrom = Just from, iqTo = Just to, iqID = Just id }))
	| fromString "CHEOGRAMCREATE%" `T.isPrefixOf` id = do
		fmap (((mkStanzaRec $ mkSMS componentJid smsJid (mconcat [fromString "* You have created ", bareTxt from])):) . concat . toList) $
			forM (parseJID $ bareTxt to <> fromString "/create") $
				queryDisco from
componentStanza (ComponentContext { componentJid }) (ReceivedIQ (IQ { iqType = typ, iqTo = Just to@(JID { jidNode = Just toNode }), iqPayload = Just p }))
	| typ `elem` [IQResult, IQError],
	  Just idAndResource <- T.stripPrefix (s"CHEOGRAM%query-then-send-command-list%") . strResource =<< jidResource to,
	  Just (iqId, resource) <- readZ $ T.unpack $ unescapeJid idAndResource,
	  Just routeTo <- parseJID (unescapeJid (strNode toNode) ++ if T.null resource then mempty else s"/" ++ resource) =
		if typ == IQError then do
			return [mkStanzaRec $ commandList componentJid iqId componentJid routeTo []]
		else do
			let items = isNamed (s"{http://jabber.org/protocol/disco#items}item") =<< elementChildren p
			return [mkStanzaRec $ commandList componentJid iqId componentJid routeTo items]
componentStanza (ComponentContext { db, componentJid, sendIQ }) (ReceivedIQ (IQ { iqType = IQError, iqTo = Just to@(JID { jidNode = Just toNode }), iqFrom = Just from }))
	| fmap strResource (jidResource to) == Just (s"CHEOGRAM%query-then-send-presence"),
	  Just routeTo <- parseJID (unescapeJid (strNode toNode)),
	  Just fromNode <- jidNode from,
	  Just routeFrom <- parseJID (strNode fromNode ++ s"@" ++ formatJID componentJid) = do
		telFeatures <- getTelFeatures db componentJid sendIQ routeTo
		return [ mkStanzaRec $ telAvailable routeFrom routeTo telFeatures ]
componentStanza (ComponentContext { db, componentJid, sendIQ }) (ReceivedIQ (IQ { iqType = IQResult, iqTo = Just to@(JID { jidNode = Just toNode }), iqFrom = Just from, iqPayload = Just p }))
	| Just idAndResource <- T.stripPrefix (s"CHEOGRAM%query-then-send-ack%") . strResource =<< jidResource to,
	  Just (messageId, resource) <- readZ $ T.unpack $ unescapeJid idAndResource,
	  [query] <- isNamed (fromString "{http://jabber.org/protocol/disco#info}query") p,
	  Just routeTo <- parseJID (unescapeJid (strNode toNode) ++ if T.null resource then mempty else s"/" ++ resource),
	  Just fromNode <- jidNode from,
	  Just routeFrom <- parseJID (strNode fromNode ++ s"@" ++ formatJID componentJid) =
		let features = mapMaybe (attributeText (fromString "var")) $ isNamed (fromString "{http://jabber.org/protocol/disco#info}feature") =<< elementChildren query in
		if (s"urn:xmpp:receipts") `elem` features then do
			return []
		else do
			return [mkStanzaRec $ deliveryReceipt messageId routeFrom routeTo]
	| fmap strResource (jidResource to) == Just (s"CHEOGRAM%query-then-send-presence"),
	  [query] <- isNamed (fromString "{http://jabber.org/protocol/disco#info}query") p,
	  Just routeTo <- parseJID (unescapeJid (strNode toNode)),
	  Just fromNode <- jidNode from,
	  Just routeFrom <- parseJID (strNode fromNode ++ s"@" ++ formatJID componentJid) = do
		telFeatures <- getTelFeatures db componentJid sendIQ routeTo
		return [
				mkStanzaRec $ telAvailable routeFrom routeTo $ (telFeatures ++) $ mapMaybe (attributeText (fromString "var")) $
				isNamed (fromString "{http://jabber.org/protocol/disco#info}feature") =<< elementChildren query
			]
	| [query] <- isNamed (fromString "{http://jabber.org/protocol/disco#info}query") p = do
		let vars = mapMaybe (attributeText (fromString "var")) $
			isNamed (fromString "{http://jabber.org/protocol/disco#info}feature") =<< elementChildren query
		let muc_membersonly = fromEnum $ fromString "muc_membersonly" `elem` vars
		True <- TC.runTCM $ TC.put db (T.unpack (formatJID from) <> "\0muc_membersonly") muc_membersonly
		if (fmap strResource (jidResource to) == Just (fromString "create")) then do
			regJid <- tcGetJID db to "registered"
			fmap (concat . toList) $ forM ((,) <$> regJid <*> parseJID (bareTxt to)) $ \(jid, to) ->
				sendInvite db jid (Invite from to Nothing Nothing)
		else
			return []
componentStanza _ (ReceivedIQ (iq@IQ { iqType = IQGet, iqFrom = Just from, iqTo = Just to, iqPayload = Just p }))
	| not $ null $ isNamed (fromString "{urn:xmpp:ping}ping") p = do
		return [mkStanzaRec $ iq {
			iqTo = Just from,
			iqFrom = Just to,
			iqType = IQResult,
			iqPayload = Nothing
		}]
componentStanza (ComponentContext { db, smsJid = maybeSmsJid, componentJid }) (ReceivedIQ (iq@IQ { iqType = typ, iqFrom = Just from }))
	| fmap strResource (jidResource =<< iqTo iq) /= Just (s"capsQuery") = do
	let resourceSuffix = maybe mempty (s"/"++) $ fmap strResource (jidResource from)
	maybeRoute <- TC.runTCM $ TC.get db (T.unpack (bareTxt from) ++ "\0direct-message-route")
	case (fmap fromString maybeRoute, parseJID $ escapeJid (bareTxt from) ++ s"@" ++ formatJID componentJid ++ resourceSuffix) of
		(Just route, Just routeFrom) -> do
			return [mkStanzaRec $ iq {
				iqFrom = Just routeFrom,
				iqTo = parseJID $ (maybe mempty (++s"@") $ strNode <$> (jidNode =<< maybeSmsJid)) ++ route
			}]
		_ | typ `elem` [IQGet, IQSet] -> do
			return [mkStanzaRec $ iqNotImplemented iq]
		_ | typ == IQError, Just smsJid <- maybeSmsJid -> do
			log "IQ ERROR" iq
			return [mkStanzaRec $ mkSMS componentJid smsJid (fromString "Error while querying or configuring " <> formatJID from)]
		_ -> log "IGNORE BOGUS REPLY (no route)" iq >> return []
componentStanza _ s = do
	log "UNKNOWN STANZA" s
	return []

participantJid payloads =
	listToMaybe $ mapMaybe (parseJID <=< attributeText (fromString "jid")) $
	isNamed (fromString "{http://jabber.org/protocol/muc#user}item") =<<
	elementChildren =<<
	isNamed (fromString "{http://jabber.org/protocol/muc#user}x") =<< payloads

cacheHTTP :: (Unexceptional m) => FilePath -> Text -> m (Either IOError FilePath)
cacheHTTP jingleStore url =
	UIO.fromIO' (userError . show) $
	HTTP.get (encodeUtf8 url) $ \response body -> UIO.runEitherIO $
		if HTTP.getStatusCode response == 200 then
			fmap (fmap (\(fp,_,_,_) -> fp)) $
			Jingle.storeChunks Nothing jingleStore
			(escapeURIString isAlpha (textToString url))
			(hush <$> UIO.fromIO (fromMaybe mempty <$> Streams.read body))
		else
			return $ Left $ userError "Response was not 200 OK"

cacheOneOOB :: (Unexceptional m) => ([StatsD.Stat] -> m ()) -> FilePath -> Text -> XML.Element -> m (Maybe (Text, Text), XML.Element)
cacheOneOOB pushStatsd jingleStore jingleStoreURL oob
	| [url] <- (mconcat . XML.elementText) <$> urls = do
		cacheResult <- cacheHTTP jingleStore url
		case cacheResult of
			Left err -> do
				pushStatsd [StatsD.stat ["cache", "oob", "failure"] 1 "c" Nothing]
				log "cacheOneOOB" err
				return (Nothing, oob)
			Right path ->
				pushStatsd [StatsD.stat ["cache", "oob", "success"] 1 "c" Nothing] >>
				let
					ext = T.takeWhileEnd (\c -> c /= '.' && c /= '/') url
					extSuffix = if T.length ext <= 4 then s"." ++ ext else mempty
					url' = jingleStoreURL ++ (T.takeWhileEnd (/='/') $ fromString path) ++ extSuffix
				in
				return (
					Just (url, url'),
					oob {
						XML.elementNodes =
							map XML.NodeElement
							(mkElement urlName url' : rest)
					}
				)
	| otherwise = do
		log "cacheOneOOB MALFORMED" oob
		return (Nothing, oob)
	where
	urlName = s"{jabber:x:oob}url"
	(urls, rest) = partition (\el -> XML.elementName el == urlName) (elementChildren oob)

cacheOOB :: (Unexceptional m) => ([StatsD.Stat] -> m ()) -> FilePath -> Text -> XMPP.Message -> m XMPP.Message
cacheOOB pushStatsd jingleStore jingleStoreURL m@(XMPP.Message { XMPP.messagePayloads = payloads }) = do
	(replacements, oobs') <- unzip <$> mapM (cacheOneOOB pushStatsd jingleStore jingleStoreURL) oobs
	let body' =
		(mkElement bodyName .: foldl (\body (a, b) -> T.replace a b body)) <$>
		(map (mconcat . XML.elementText) body) <*> pure (catMaybes replacements)
	return $ m { XMPP.messagePayloads = noOobsNoBody ++ oobs' ++ body' }
	where
	oobName = s"{jabber:x:oob}x"
	bodyName = s"{jabber:component:accept}body"
	(body, noOobsNoBody) = partition (\el -> XML.elementName el == bodyName) noOobs
	(oobs, noOobs) = partition (\el -> XML.elementName el == oobName) payloads

component db redis pushStatsd backendHost did maybeAvatar cacheOOB sendIQ iqReceiver adhocBotMessage toRoomPresences toRejoinManager toJoinPartDebouncer toComponent toStanzaProcessor processDirectMessageRouteConfig jingleHandler componentJid registrationJids conferenceServers = do
	sendThread <- forkXMPP $ forever $ flip catchError (log "component EXCEPTION") $ do
		stanza <- liftIO $ atomically $ readTChan toComponent

		let tags = maybe "" (";domain=" ++) (textToString . strDomain . jidDomain <$> stanzaTo stanza)
		pushStatsd [StatsD.stat ["stanzas", "out" ++ tags] 1 "c" Nothing]

		case (stanzaFrom stanza, stanzaTo stanza) of
			(Just from, Just to)
				| strDomain (jidDomain to) == backendHost,
				  from == componentJid ->
					forM_ (tcKey to "welcomed") $ \welcomedKey -> do
						welcomed <- maybe False toEnum <$> liftIO (TC.runTCM $ TC.get db welcomedKey)
						unless welcomed $ do
							putStanza $ mkSMS componentJid to $ fromString "Welcome to CheoGram! You can chat with groups of friends (one at a time), by replying to this number. Reply with /help to learn more or visit cheogram.com"
							tcPut db to "welcomed" (fromEnum True)
			_ -> return ()

		putStanza stanza

	recvThread <- forkXMPP $ forever $ flip catchError (log "component read EXCEPTION") $
		(atomicUIO . writeTChan toStanzaProcessor) =<< getStanza

	flip catchError (\e -> liftIO (log "component part 2 EXCEPTION" e >> killThread sendThread >> killThread recvThread)) $ forever $ do
		stanza <- atomicUIO $ readTChan toStanzaProcessor
		let tags = maybe "" (";domain=" ++) (textToString . strDomain . jidDomain <$> stanzaFrom (receivedStanza stanza))
		pushStatsd [StatsD.stat ["stanzas", "in" ++ tags] 1 "c" Nothing]
		liftIO $ forkIO $ case stanza of
			(ReceivedPresence p@(Presence { presenceType = PresenceAvailable, presenceFrom = Just from, presenceTo = Just to }))
				| Just returnFrom <- parseJID (bareTxt to ++ s"/capsQuery") ->
				let
					cheogramBareJid = escapeJid (bareTxt from) ++ s"@" ++ formatJID componentJid
					caps = child (s"{http://jabber.org/protocol/caps}c") p
					show = maybe mempty mconcat $ elementText <$> child (s"{jabber:component:accept}show") p
					priority = fromMaybe 0 $ (readZ . textToString . mconcat =<< elementText <$> child (s"{jabber:component:accept}priority") p)
					pavailableness = availableness show priority
				in do
				-- Caps?
				case (XML.attributeText (s"ver") =<< caps, XML.attributeText (s"node") =<< caps) of
				-- Yes: write ver to <barejid>/resource and <cheoagramjid>/resource
					(Just ver, Just node) -> do
						let bver = Base64.decodeLenient $ encodeUtf8 ver
						let val = LZ.toStrict $ Builder.toLazyByteString (Builder.word16BE pavailableness ++ Builder.byteString bver)
						Right exists <- Redis.runRedis redis $ do
							Redis.hset (encodeUtf8 $ bareTxt from) (encodeUtf8 $ maybe mempty strResource $ jidResource from) val
							Redis.hset (encodeUtf8 $ cheogramBareJid) (encodeUtf8 $ maybe mempty strResource $ jidResource from) val
					-- ver in redis?
							Redis.exists bver
					-- Yes: done
					-- No: send disco query, with node
						when (not exists) $ sendToComponent . mkStanzaRec =<< queryDiscoWithNode (Just $ node ++ s"#" ++ ver) from returnFrom
				-- No: write only availableness to redis. send disco query, no node
					_ -> do
						let val = LZ.toStrict $ Builder.toLazyByteString (Builder.word16BE pavailableness)
						void $ Redis.runRedis redis $ do
							Redis.hset (encodeUtf8 $ bareTxt from) (encodeUtf8 $ maybe mempty strResource $ jidResource from) val
							Redis.hset (encodeUtf8 $ cheogramBareJid) (encodeUtf8 $ maybe mempty strResource $ jidResource from) val
						mapM_ sendToComponent =<< queryDisco from returnFrom
			(ReceivedPresence (Presence { presenceType = PresenceUnavailable, presenceFrom = Just from })) -> do
				let cheogramBareJid = escapeJid (bareTxt from) ++ s"@" ++ formatJID componentJid
				-- Delete <barejid>/resource and <cheogramjid>/resource
				void $ Redis.runRedis redis $ do
					Redis.hdel (encodeUtf8 $ bareTxt from) [encodeUtf8 $ maybe mempty strResource $ jidResource from]
					Redis.hdel (encodeUtf8 $ cheogramBareJid) [encodeUtf8 $ maybe mempty strResource $ jidResource from]
			(ReceivedIQ iq@(IQ { iqType = IQResult, iqFrom = Just from }))
				| Just query <- child (s"{http://jabber.org/protocol/disco#info}query") iq -> do
				let cheogramBareJid = escapeJid (bareTxt from) ++ s"@" ++ formatJID componentJid
				let bver = discoToCapsHash query
				-- Write <ver> with the list of features
				void $ Redis.runRedis redis $ do
					Redis.sadd bver (encodeUtf8 <$> discoVars query)
				-- Write ver to <barejid>/resource and <cheogramjid>/resource
					Right ravailableness <- (fmap . fmap) (maybe (BS.pack [0,0]) (BS.take 2)) $ Redis.hget (encodeUtf8 $ bareTxt from) (encodeUtf8 $ maybe mempty strResource $ jidResource from)
					let val = ravailableness ++ bver
					Redis.hset (encodeUtf8 $ bareTxt from) (encodeUtf8 $ maybe mempty strResource $ jidResource from) val
					Redis.hset (encodeUtf8 $ cheogramBareJid) (encodeUtf8 $ maybe mempty strResource $ jidResource from) val
			_ -> return ()
		flip forkFinallyXMPP (either (log "RECEIVE ONE" . show) return) $ case (stanzaFrom $ receivedStanza stanza, stanzaTo $ receivedStanza stanza, mapToBackend backendHost =<< stanzaTo (receivedStanza stanza), fmap strNode . jidNode =<< stanzaTo (receivedStanza stanza), stanza) of
			(_, Just to, _, _, ReceivedIQ iq@(IQ { iqType = typ }))
				| typ `elem` [IQResult, IQError],
				  (strResource <$> jidResource to) `elem` map Just [s"adhocbot", s"IQMANAGER"] ->
					iqReceiver iq
			(Just from, Just to, _, _, ReceivedMessage (Message { messageType = MessageError }))
				| strDomain (jidDomain from) == backendHost,
				  to == componentJid ->
					log "backend error" stanza
			(Just from, Just to, _, _, ReceivedMessage m)
				| strDomain (jidDomain from) == backendHost,
				  to == componentJid,
				  Just txt <- getBody "jabber:component:accept" m,
				  Just cheoJid <- mapToComponent from,
				  fmap strNode (jidNode from) /= Just did ->
					liftIO (mapM_ sendToComponent =<< processSMS db componentJid conferenceServers from cheoJid txt)
			(Just from, Just to, Nothing, Just localpart, ReceivedMessage m)
				| Just txt <- getBody "jabber:component:accept" m,
				  (T.length txt == 144 || T.length txt == 145) && (s"CHEOGRAM") `T.isPrefixOf` txt -> liftIO $ do -- the length of our token messages
					log "POSSIBLE TOKEN" (from, to, txt)
					maybeRoute <- TC.runTCM $ TC.get db (T.unpack (unescapeJid localpart) ++ "\0direct-message-route")
					when (Just (strDomain $ jidDomain from) == fmap fromString maybeRoute || bareTxt from == unescapeJid localpart) $ do
						maybeToken <- TC.runTCM $ TC.get db (T.unpack (unescapeJid localpart) ++ "\0addtoken")
						case (fmap (first parseJID) (readZ =<< maybeToken), parseJID $ unescapeJid localpart) of
							(Just (Just cheoJid, token), Just owner) | (s"CHEOGRAM"++token) == txt -> do
								log "SET OWNER" (cheoJid, owner)

								True <- TC.runTCM (TC.put db (T.unpack (bareTxt owner) ++ "\0cheoJid") (T.unpack $ formatJID cheoJid))

								owners <- (fromMaybe [] . (readZ =<<)) <$>
									maybe (return Nothing) (TC.runTCM . TC.get db) (tcKey cheoJid "owners")
								tcPut db cheoJid "owners" (show $ (T.unpack $ bareTxt owner) : owners)

							_ -> log "NO TOKEN FOUND, or mismatch" maybeToken
			(Just from, Just to, Nothing, Just localpart, _)
				| Just multipleTo <- mapM localpartToURI (T.split (==',') localpart),
				  ReceivedMessage m <- stanza,
				  Just backendJid <- parseJID backendHost -> liftIO $
					let m' = m { messagePayloads = messagePayloads m ++ [
						Element (s"{http://jabber.org/protocol/address}addresses") [] $ map (\oneto ->
							NodeElement $ Element (s"{http://jabber.org/protocol/address}address") [
								(s"{http://jabber.org/protocol/address}type", [ContentText $ s"to"]),
								(s"{http://jabber.org/protocol/address}uri", [ContentText oneto])
							] []
						) multipleTo
					] } in
					-- TODO: should check if backend supports XEP-0033
					-- TODO: fallback for no-backend case should work
					mapM_ sendToComponent =<< componentMessage db componentJid m' Nothing (bareTxt from) (strResource <$> jidResource from) backendJid (getBody "jabber:component:accept" m')
				| (s"sip.cheogram.com") == strDomain (jidDomain from) -> liftIO $ do
					let (toResource, fromResource)
						| Just toResource <- T.stripPrefix (s"CHEOGRAM%outbound-sip%") =<< (strResource <$> jidResource to) = (toResource, s"tel")
						| otherwise = (fromMaybe mempty (strResource <$> jidResource to), s"sip:" ++ escapeJid (formatJID from))
					case (mapLocalpartToBackend (formatJID componentJid) =<< sanitizeSipLocalpart (maybe mempty strNode $ jidNode from), parseJID (unescapeJid localpart ++ s"/" ++ toResource)) of
						(Just componentFrom, Just routeTo) -> liftIO $ do
							Just componentFromSip <- return $ parseJID (formatJID componentFrom ++ s"/" ++ fromResource)
							sendToComponent $ mkStanzaRec $ receivedStanza $ receivedStanzaFromTo componentFromSip routeTo stanza
						_ ->
							sendToComponent $ stanzaError stanza $
								Element (fromString "{jabber:component:accept}error")
								[(fromString "{jabber:component:accept}type", [ContentText $ fromString "cancel"])]
								[NodeElement $ Element (fromString "{urn:ietf:params:xml:ns:xmpp-stanzas}item-not-found") [] []]
			(Just from, Just to, Nothing, Just localpart, _)
				| Nothing <- mapM localpartToURI (T.split (==',') $ fromMaybe mempty $ fmap strNode $ jidNode to),
				  fmap (((s"CHEOGRAM%") `T.isPrefixOf`) . strResource) (jidResource to) /= Just True -> liftIO $ do
					let toResourceSuffix = maybe mempty (s"/"++) (strResource <$> jidResource to)
					maybeRoute <- TC.runTCM $ TC.get db (T.unpack (unescapeJid localpart) ++ "\0direct-message-route")
					case (fmap fromString maybeRoute, parseJID (unescapeJid localpart ++ toResourceSuffix), mapToComponent from) of
						(Just route, Just routeTo, Just componentFrom) | route == strDomain (jidDomain from) ->
							(sendToComponent . receivedStanza) =<< mapReceivedMessageM (UIO.lift . cacheOOB) (receivedStanzaFromTo componentFrom routeTo stanza)
						_ | Just jid <- (`telToJid` formatJID componentJid) =<< strNode <$> jidNode to -> do
							sendToComponent $ stanzaError stanza $
								Element (fromString "{jabber:component:accept}error")
								[(fromString "{jabber:component:accept}type", [ContentText $ fromString "cancel"])]
								[
									NodeElement $ Element (fromString "{urn:ietf:params:xml:ns:xmpp-stanzas}gone") []
										[NodeContent $ ContentText $ formatJID jid],
									NodeElement $ Element (fromString "{urn:ietf:params:xml:ns:xmpp-stanzas}text")
										[(fromString "xml:lang", [ContentText $ fromString "en"])]
										[NodeContent $ ContentText $ fromString "JID must include country code: " <> formatJID jid]
								]
						  | otherwise ->
							sendToComponent $ stanzaError stanza $
								Element (fromString "{jabber:component:accept}error")
								[(fromString "{jabber:component:accept}type", [ContentText $ fromString "cancel"])]
								[NodeElement $ Element (fromString "{urn:ietf:params:xml:ns:xmpp-stanzas}item-not-found") [] []]
			(mfrom, to, backendTo, _, _)
				| Just sipJid <- parseJID =<< T.stripPrefix (s"sip:") =<< (unescapeJid . strResource <$> (jidResource =<< to)),
				  Just from <- mfrom,
				  resourceSuffix <- maybe mempty (s"/"++) (fmap strResource (jidResource =<< mfrom)),
				  Just useFrom <- parseJID $ (escapeJid $ bareTxt from) ++ s"@" ++ formatJID componentJid ++ resourceSuffix -> do
					liftIO $ sendToComponent $ mkStanzaRec $ receivedStanza $ receivedStanzaFromTo useFrom sipJid stanza
				| ReceivedIQ (iq@IQ { iqType = IQSet, iqPayload = Just p }) <- stanza,
				  (nameNamespace $ elementName p) `elem` [Just (s"urn:xmpp:jingle:1"), Just (s"http://jabber.org/protocol/ibb")] -> do
					jingleHandler iq
				| otherwise -> liftIO $
					mapM_ sendToComponent =<< componentStanza (ComponentContext db backendTo registrationJids adhocBotMessage cacheOOB toRoomPresences toRejoinManager toJoinPartDebouncer processDirectMessageRouteConfig componentJid sendIQ maybeAvatar) stanza
	where
	mapToComponent = mapToBackend (formatJID componentJid)
	sendToComponent = atomically . writeTChan toComponent

	stanzaError (ReceivedMessage m) errorPayload =
		mkStanzaRec $ m {
			messageFrom = messageTo m,
			messageTo = messageFrom m,
			messageType = MessageError,
			messagePayloads = messagePayloads m ++ [errorPayload]
		}
	stanzaError (ReceivedPresence p) errorPayload =
		mkStanzaRec $ p {
			presenceFrom = presenceTo p,
			presenceTo = presenceFrom p,
			presenceType = PresenceError,
			presencePayloads = presencePayloads p ++ [errorPayload]
		}
	stanzaError (ReceivedIQ iq) errorPayload =
		mkStanzaRec $ iq {
			iqFrom = iqTo iq,
			iqTo = iqFrom iq,
			iqType = IQError,
			iqPayload = Just errorPayload
		}

	receivedStanzaFromTo from to (ReceivedMessage m) = ReceivedMessage $ m {
			messageFrom = Just from,
			messageTo = Just to
		}
	receivedStanzaFromTo from to (ReceivedPresence p) = ReceivedPresence $ p {
			presenceFrom = Just from,
			presenceTo = Just to
		}
	receivedStanzaFromTo from to (ReceivedIQ iq) = ReceivedIQ $ rewriteJingleInitiatorResponder $ iq {
			iqFrom = Just from,
			iqTo = Just to
		}

	receivedStanza (ReceivedMessage m) = mkStanzaRec m
	receivedStanza (ReceivedPresence p) = mkStanzaRec p
	receivedStanza (ReceivedIQ iq) = mkStanzaRec iq

-- Jingle session-initiate and session-accept iqs contain the sending JID
-- again for some reason, so make sure we keep those the same
rewriteJingleInitiatorResponder iq
	| Just jingle <- child (s"{urn:xmpp:jingle:1}jingle") iq = iq {
			XMPP.iqPayload = Just $ jingle {
				XML.elementAttributes = map initiatorResponder (XML.elementAttributes jingle)
			}
		}
	| otherwise = iq
	where
	initiatorResponder (name, content)
		| name == s"initiator" = (name, [XML.ContentText $ maybe (s"") XMPP.formatJID (XMPP.iqFrom iq)])
		| name == s"responder" = (name, [XML.ContentText $ maybe (s"") XMPP.formatJID (XMPP.iqFrom iq)])
		| otherwise = (name, content)

groupTextPorcelein :: Text -> Message -> Maybe Message
groupTextPorcelein host m@(Message { messagePayloads = p, messageFrom = Just from })
	| [addresses] <- isNamed (s"{http://jabber.org/protocol/address}addresses") =<< p,
	  [body] <- isNamed (s"{jabber:component:accept}body") =<< p,
	  (jids, uris) <- partition (maybe False (const True) . headZ . hasAttribute (s"jid"))
			(hasAttributeText (s"type") (`elem` [s"to", s"cc"]) =<<
			isNamed (s"{http://jabber.org/protocol/address}address") =<< elementChildren addresses),
	  [Just to] <- (proxiedJidToReal <=< parseJID <=< attributeText (s"jid")) <$> jids,
	  Just fromTel <- strNode <$> jidNode from,
	  Just tels <- fmap (textToString fromTel:) $ mapM (fmap uriPath . parseURI . textToString <=< attributeText (s"uri")) uris =
		Just $ m {
			messageTo = Just to,
			messageFrom = parseJID (fromString (intercalate "," (sort tels)) ++ (s"@") ++ host),
			messagePayloads = body { elementNodes = (NodeContent $ ContentText $ s"<xmpp:") : (NodeContent $ ContentText fromTel) : (NodeContent $ ContentText $ s"@") : (NodeContent $ ContentText host) : (NodeContent $ ContentText $ s"> ") : elementNodes body } :
				filter (`notElem` [addresses, body]) p
		}
groupTextPorcelein _ _ = Nothing

proxiedJidToReal :: JID -> Maybe JID
proxiedJidToReal jid =
	parseJID =<< fmap (maybe id (\r -> (++ (s"/" ++ r))) resource) bare
	where
	resource = strResource <$> jidResource jid
	bare = unescapeJid . strNode <$> jidNode jid

mapToBackend backendHost (JID { jidNode = Just node }) = mapLocalpartToBackend backendHost (strNode node)
mapToBackend backendHost (JID { jidNode = Nothing }) = parseJID backendHost

mapLocalpartToBackend backendHost localpart
	| Just ('+', tel) <- T.uncons localpart,
	  T.all isDigit tel = result
	| Just _ <- parsePhoneContext localpart = result
	| otherwise = Nothing
	where
	result = parseJID (localpart ++ s"@" ++ backendHost)

localpartToURI localpart
	| Just ('+', tel) <- T.uncons localpart,
	  T.all isDigit tel = result
	| Just _ <- parsePhoneContext localpart = result
	| otherwise = Nothing
	where
	result = Just (s"sms:" ++ localpart)

isE164 fullTel
	| Just ('+',e164) <- T.uncons fullTel = T.all isDigit e164
	| otherwise = False

normalizeTel fullTel
	| isE164 fullTel = Just fullTel
	| T.length tel == 10 = Just (s"+1" ++ tel)
	| T.length tel == 11, s"1" `T.isPrefixOf` tel = Just (T.cons '+' tel)
	| T.length tel == 5 || T.length tel == 6 = Just (tel ++ s";phone-context=ca-us.phone-context.soprani.ca")
	| otherwise = Nothing
	where
	tel = T.filter isDigit fullTel

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

data Command = Help | Create Text | Join JID | JoinInvited | JoinInvitedWrong | Debounce Int | Send Text | Who | List | Leave | InviteCmd JID | SetNick Text | Whisper JID Text | AddJid JID | DelJid JID | Jids
	deriving (Show, Eq)

parseCommand txt room nick componentJid
	| Just jid <- stripCIPrefix (fromString "/invite ") txt =
		InviteCmd <$> (
			(maybeStripProxy <$> parseJIDrequireNode jid) <|>
			telToJid jid (formatJID componentJid)
		)
	| Just room <- stripCIPrefix (fromString "/join ") txt =
		Join <$> (parseJID (room <> fromString "/" <> nick) <|> parseJID room)
	| Just addjid <- stripCIPrefix (fromString "/addjid ") txt =
		AddJid <$> parseJID addjid
	| Just deljid <- stripCIPrefix (fromString "/deljid ") txt =
		DelJid <$> parseJID deljid
	| citxt == fromString "/jids" = Just Jids
	| Just t <- stripCIPrefix (fromString "/create ") txt = Just $ Create t
	| Just nick <- stripCIPrefix (fromString "/nick ") txt = Just $ SetNick nick
	| Just input <- stripCIPrefix (fromString "/msg ") txt =
		let (to, msg) = T.breakOn (fromString " ") input in
		Whisper <$> (
			(maybeStripProxy <$> parseJIDrequireNode to) <|>
			telToJid to (formatJID componentJid) <|>
			(parseJID =<< fmap (\r -> bareTxt r <> fromString "/" <> to) room)
		) <*> pure msg
	| Just stime <- stripCIPrefix (fromString "/debounce ") txt,
	  Just time <- readMay stime = Just $ Debounce time
	| citxt == fromString "/join" = Just JoinInvited
	| citxt == fromString "join" = Just JoinInvitedWrong
	| citxt == fromString "/leave" = Just Leave
	| citxt == fromString "/part" = Just Leave
	| citxt == fromString "/who" = Just Who
	| citxt == fromString "/list" = Just List
	| citxt == fromString "/help" = Just Help
	| otherwise = Just $ Send txt
	where
	maybeStripProxy jid
		| jidDomain jid == jidDomain (componentJid) = fromMaybe jid $ proxiedJidToReal jid
		| otherwise = jid
	citxt = CI.mk txt

getMessage (ReceivedMessage m) = Just m
getMessage _ = Nothing

sendToRoom cheoJid room msg = do
	uuid <- (fmap.fmap) UUID.toString UUID.nextUUID
	return [mkStanzaRec $ (emptyMessage MessageGroupChat) {
		messageTo = parseJID $ bareTxt room,
		messageFrom = Just cheoJid,
		messageID = Just $ fromString ("CHEOGRAM%" <> fromMaybe "UUIDFAIL" uuid),
		messagePayloads = [Element (fromString "{jabber:component:accept}body") [] [NodeContent $ ContentText msg]]
	}]

leaveRoom :: TC.HDB -> JID -> String -> IO [StanzaRec]
leaveRoom db cheoJid reason = do
	existingRoom <- tcGetJID db cheoJid "joined"
	return $ (flip map) (toList existingRoom) $ \leaveRoom ->
		mkStanzaRec $ (emptyPresence PresenceUnavailable) {
			presenceTo = Just leaveRoom,
			presenceFrom = Just cheoJid,
			presencePayloads = [Element (fromString "{jabber:component:accept}status") [] [NodeContent $ ContentText $ fromString reason]]
		}

joinRoom db cheoJid room =
	rejoinRoom db cheoJid room False

rejoinRoom db cheoJid room rejoin = do
	password <- maybe (return Nothing) (TC.runTCM . TC.get db) (tcKey cheoJid (T.unpack (bareTxt room) <> "\0muc_roomsecret"))
	let pwEl = maybe [] (\pw -> [
			NodeElement $ Element (fromString "{http://jabber.org/protocol/muc}password") [] [NodeContent $ ContentText $ fromString pw]
		]) password

	uuid <- fromMaybe "UUIDFAIL" <$> (fmap.fmap) (fromString . UUID.toString) UUID.nextUUID
	return [mkStanzaRec $ (emptyPresence PresenceAvailable) {
		presenceID = Just $ fromString $ (if rejoin then "CHEOGRAMREJOIN%" else "CHEOGRAMJOIN%") <> uuid,
		presenceTo = Just room,
		presenceFrom = Just cheoJid,
		presencePayloads = [Element (fromString "{http://jabber.org/protocol/muc}x") [] ([
			NodeElement $ Element (fromString "{http://jabber.org/protocol/muc}history") [(fromString "{http://jabber.org/protocol/muc}maxchars", [ContentText $ fromString "0"])] []
		] <> pwEl)]
	}]

addMUCOwner room from jid = do
	uuid <- (fmap.fmap) UUID.toString UUID.nextUUID
	return [mkStanzaRec $ (emptyIQ IQSet) {
		iqTo = Just room,
		iqFrom = Just from,
		iqID = fmap fromString uuid,
		iqPayload = Just $ Element (fromString "{http://jabber.org/protocol/muc#admin}admin") [] [
			NodeElement $ Element (fromString "{http://jabber.org/protocol/muc#admin}item")
				[
					(fromString "{http://jabber.org/protocol/muc#admin}affiliation", [ContentText $ fromString "owner"]),
					(fromString "{http://jabber.org/protocol/muc#admin}jid", [ContentText $ formatJID jid])
				] []
		]
	}]

createRoom :: JID -> [Text] -> JID -> Text -> IO [StanzaRec]
createRoom componentJid (server:otherServers) cheoJid name
	-- First we check if this room exists on the server already
	| Just t <- to = queryDisco t jid
	| otherwise = return []
	where
	to = parseJID $ name <> fromString "@" <> server
	Just jid = parseJID $ fromString "create@" <> formatJID componentJid <> fromString "/" <> intercalate (fromString "|") ((formatJID cheoJid):name:otherServers)
createRoom _ [] _ _ = return []

mucShortMatch tel short muc =
	node == short || T.stripSuffix (fromString "_" <> tel) node == Just short
	where
	node = maybe mempty strNode (jidNode =<< parseJID muc)

sendInvite db to (Invite { inviteMUC = room, inviteFrom = from }) = do
	membersonly <- maybe False toEnum <$> TC.runTCM (TC.get db (T.unpack (bareTxt room) <> "\0muc_membersonly"))
	-- Try to add everyone we invite as an owner also
	(++) <$> (if membersonly then addMUCOwner room from to else return []) <*>
		return [
				mkStanzaRec $ (emptyMessage MessageNormal) {
					messageTo = Just room,
					messageFrom = Just from,
					messagePayloads = [
						Element (fromString "{http://jabber.org/protocol/muc#user}x") [] [
							NodeElement $ Element (fromString "{http://jabber.org/protocol/muc#user}invite") [
								(fromString "{http://jabber.org/protocol/muc#user}to", [ContentText $ formatJID to])
							] []
						]
					]
				},

				mkStanzaRec $ (emptyMessage MessageNormal) {
					messageTo = Just to,
					messageFrom = Just from,
					messagePayloads = [
						Element (fromString "{jabber:x:conference}x") [
							(fromString "{jabber:x:conference}jid", [ContentText $ formatJID room])
						] [],
						Element (fromString "{jabber:component:accept}body") []
							[NodeContent $ ContentText $ mconcat [formatJID from, fromString " has invited you to join ", formatJID room]]
					]
				}
			]

registerToGateway componentJid gatewayJid did password = return [
		mkStanzaRec $ (emptyIQ IQSet) {
			iqTo = Just gatewayJid,
			iqFrom = Just componentJid,
			iqPayload = Just $ Element (fromString "{jabber:iq:register}query") []
				[
					NodeElement $ Element (fromString "{jabber:iq:register}phone") [] [NodeContent $ ContentText did],
					NodeElement $ Element (fromString "{jabber:iq:register}password") [] [NodeContent $ ContentText password]
				]
		}
	]

processSMS db componentJid conferenceServers smsJid cheoJid txt = do
	nick <- maybe (maybe (formatJID cheoJid) strNode (jidNode cheoJid)) fromString <$> maybe (return Nothing) (TC.runTCM . TC.get db) (tcKey cheoJid "nick")
	existingRoom <- (parseJID <=< fmap bareTxt) <$> tcGetJID db cheoJid "joined"
	case parseCommand txt existingRoom nick componentJid of
		Just JoinInvited -> do
			invitedRoom <- tcGetJID db cheoJid "invited"
			let toJoin = invitedRoom >>= \jid -> parseJID (bareTxt jid <> fromString "/" <> nick)
			case toJoin of
				Just room ->
					(++) <$>
					leaveRoom db cheoJid "Joined a different room." <*>
					joinRoom db cheoJid room
				Nothing -> return [mkStanzaRec $ mkSMS componentJid smsJid (fromString "You have not recently been invited to a group")]
		Just JoinInvitedWrong
			| Just room <- existingRoom -> sendToRoom cheoJid room (fromString "Join")
			| otherwise -> do
				invitedRoom <- tcGetJID db cheoJid "invited"
				let toJoin = invitedRoom >>= \jid -> parseJID (bareTxt jid <> fromString "/" <> nick)
				case toJoin of
					Just room ->
						fmap ((mkStanzaRec $ mkSMS componentJid smsJid (fromString "I think you meant \"/join\", trying anyway...")):)
						(joinRoom db cheoJid room)
					Nothing -> return [mkStanzaRec $ mkSMS componentJid smsJid (fromString "You have not recently been invited to a group")]
		Just (Create name) -> do
			servers <- shuffleM conferenceServers
			roomCreateStanzas <- createRoom componentJid servers cheoJid name
			if null roomCreateStanzas then
				return [mkStanzaRec $ mkSMS componentJid smsJid (fromString "Invalid group name")]
			else
				return roomCreateStanzas
		Just (Join room) -> do
			leaveRoom db cheoJid "Joined a different room."
			bookmarks <- fmap (fromMaybe [] . (readZ =<<)) (maybe (return Nothing) (TC.runTCM . TC.get db) (tcKey cheoJid "bookmarks"))
			let tel = maybe mempty strNode (jidNode cheoJid)
			joinRoom db cheoJid $
				fromMaybe room $ parseJID =<< fmap (<> fromString "/" <> nick)
				(find (mucShortMatch tel (strDomain $ jidDomain room)) bookmarks)
		Just Leave -> leaveRoom db cheoJid "Typed /leave"
		Just Who -> do
			let f = fst :: (String, Maybe String) -> String
			let snick = T.unpack nick
			let room = maybe "" (T.unpack . bareTxt) existingRoom
			presence <- fmap (fromMaybe [] . (readZ =<<)) (TC.runTCM $ TC.get db ("presence\0" <> room))
			let presence' = filter (/= snick) $ map f presence
			if null presence then
				return [mkStanzaRec $ mkSMS componentJid smsJid $ fromString $
						"You are not joined to a group. Reply with /help to learn more"
					]
			else
				return [mkStanzaRec $ mkSMS componentJid smsJid $ fromString $ mconcat $ [
					"You are joined to ", room,
					" as ", snick] ++ if null presence' then [] else [
					" along with\n",
					intercalate ", " presence'
				]]
		Just List -> do
			mbookmarks <- maybe (return Nothing) (TC.runTCM . TC.get db) (tcKey cheoJid "bookmarks")
			let bookmarks = fromMaybe [] $ readZ =<< mbookmarks
			return [mkStanzaRec $ mkSMS componentJid smsJid $ fromString $ "Groups you can /join\n" <> intercalate "\n" bookmarks]
		Just (InviteCmd jid)
			| Just room <- existingRoom ->
				sendInvite db jid (Invite room cheoJid Nothing Nothing)
			| otherwise -> return [mkStanzaRec $ mkSMS componentJid smsJid (fromString "You are not joined to a group. Reply with /help to learn more")]
		Just (SetNick nick) -> do
			tcPut db cheoJid "nick" (T.unpack nick)
			fmap (concat . toList) $ forM existingRoom $ \room -> do
				let toJoin = parseJID (bareTxt room <> fromString "/" <> nick)
				fmap (concat . toList) $ forM toJoin $ joinRoom db cheoJid
		Just (Whisper to msg) -> do
			uuid <- (fmap.fmap) UUID.toString UUID.nextUUID
			return [mkStanzaRec $ (emptyMessage MessageChat) {
				messageTo = Just to,
				messageFrom = Just cheoJid,
				messageID = Just $ fromString ("CHEOGRAM%" <> fromMaybe "UUIDFAIL" uuid),
				messagePayloads = [Element (fromString "{jabber:component:accept}body") [] [NodeContent $ ContentText msg]]
			}]
		Just (Send msg)
			| Just room <- existingRoom -> sendToRoom cheoJid room msg
			| otherwise -> return [mkStanzaRec $ mkSMS componentJid smsJid (fromString "You are not joined to a group")]
		Just (Debounce time) -> do
			tcPut db cheoJid "debounce" (show time)
			return []
		Just Help -> return [
				mkStanzaRec $ mkSMS componentJid smsJid $ fromString $ mconcat [
						"Invite to group: /invite phone-number\n",
						"Show group participants: /who\n",
						"Set nick: /nick nickname\n",
						"List groups: /list\n",
						"Create a group: /create short-name"
					],
				mkStanzaRec $ mkSMS componentJid smsJid $ fromString $ mconcat [
						"Join existing group: /join group-name\n",
						"Whisper to user: /msg username message\n",
						"Leave group: /leave\n",
						"More info: http://cheogram.com"
					]
			]
		Just (AddJid addjid) -> do
			token <- genToken 100
			True <- TC.runTCM $ TC.put db (T.unpack (bareTxt addjid) ++ "\0addtoken") (show (formatJID cheoJid, token))
			return $ case parseJID (formatJID componentJid ++ s"/token") of
				Just sendFrom -> [mkStanzaRec $ mkSMS sendFrom smsJid (s"CHEOGRAM" ++ token)]
				Nothing -> []
		Just (DelJid deljid) -> do
			-- Deleting a JID is much less dangerous since in the worst case SMS just go to the actual phone number
			TC.runTCM $ TC.out db (T.unpack (bareTxt deljid) ++ "\0cheoJid")

			owners <- (fromMaybe [] . (readZ =<<)) <$>
				maybe (return Nothing) (TC.runTCM . TC.get db) (tcKey cheoJid "owners")
			tcPut db cheoJid "owners" (show $ (filter (/= bareTxt deljid)) owners)

			return [mkStanzaRec $ mkSMS componentJid smsJid (bareTxt deljid ++ s" removed from your phone number")]
		Just Jids -> do
			owners <- (fromMaybe [] . (readZ =<<)) <$>
				maybe (return Nothing) (TC.runTCM . TC.get db) (tcKey cheoJid "owners")
			return [mkStanzaRec $ mkSMS componentJid smsJid $ fromString $ "JIDs owning this phone number:\n" <> intercalate "\n" owners]
		Nothing -> return [mkStanzaRec $ mkSMS componentJid smsJid (fromString "You sent an invalid message")]

syncCall chan req = do
	var <- atomically $ newEmptyTMVar
	atomically $ writeTChan chan (req var)
	atomically $ takeTMVar var

data RejoinManagerCommand =
	CheckPings      |
	PingReply   JID |
	PingError   JID |
	Joined      JID |
	ForceRejoin JID JID

data RejoinManagerState = PingSent JID | Rejoining

rejoinManager db sendToComponent componentJid toRoomPresences toRejoinManager =
	next mempty
	where
	mkMucJid muc nick = parseJID $ bareTxt muc <> fromString "/" <> nick
	ourJids muc (x,y)
		| Just (JID { jidDomain = d }) <- yJid,
		  strDomain d /= fromString componentJid = Nothing
		| otherwise = (,) <$> mkMucJid muc x <*> yJid
		where
		yJid = parseJID =<< y

	next state = atomically (readTChan toRejoinManager) >>= go state

	go state (PingReply mucJid) =
		next $! Map.delete mucJid state
	go state (PingError mucJid) = do
		forM_ (Map.lookup mucJid state) $ \x -> case x of
			PingSent cheoJid -> atomically $ writeTChan toRejoinManager (ForceRejoin mucJid cheoJid)
			_ -> return ()
		next state
	go state (Joined mucJid) =
		next $! Map.delete mucJid state
	go state (ForceRejoin mucJid cheoJid) = do
		atomically $ writeTChan toRoomPresences (StartRejoin cheoJid mucJid)
		mapM_ sendToComponent =<< rejoinRoom db cheoJid mucJid True
		next $! Map.insert mucJid Rejoining state
	go state CheckPings = do
		presenceKeys <- TC.runTCM $ TC.fwmkeys db "presence\0" maxBound
		(next =<<) $! (\x -> foldM x state (presenceKeys :: [String])) $ \state pkey -> do
			let Just muc = parseJID =<< T.stripPrefix (fromString "presence\0") (T.pack pkey)
			presences <- fmap (mapMaybe (ourJids muc) . fromMaybe [] . (readZ =<<)) (TC.runTCM $ TC.get db pkey)
			(\x -> foldM x state presences) $ \state (mucJid, cheoJid) ->
				case Map.lookup mucJid state of
					Nothing -> do
						uuid <- fromMaybe "UUIDFAIL" <$> (fmap.fmap) (fromString . UUID.toString) UUID.nextUUID
						sendToComponent $ mkStanzaRec $ (emptyIQ IQGet) {
							iqTo = Just mucJid,
							iqFrom = Just cheoJid,
							iqID = Just $ fromString $ "CHEOGRAMPING%" <> uuid,
							iqPayload = Just $ Element (fromString "{urn:xmpp:ping}ping") [] []
						}
						return $! Map.insert mucJid (PingSent cheoJid) state
					Just (PingSent _) -> do -- Timeout, rejoin
						atomically $ writeTChan toRejoinManager (ForceRejoin mucJid cheoJid)
						return state
					Just Rejoining -> -- Don't ping, we're working on it
						return state

-- tel@cheogram, from (bare is MUC, resource is nick), Maybe participantJID
data RoomPresences =
	RecordSelfJoin JID JID (Maybe JID) |
	RecordJoin JID JID (Maybe JID) |
	RecordPart JID JID |
	RecordNickChanged JID JID Text |
	Clear JID JID |
	StartRejoin JID JID |
	GetRoomPresences JID JID (TMVar [(String, Maybe String)])

roomPresences db toRoomPresences =
	forever $ atomically (readTChan toRoomPresences) >>= go
	where
	go (RecordSelfJoin cheoJid from jid) = do
		-- After a join is done we have a full presence list, remove old ones
		forM_ (tcKey cheoJid (muc from <> "\0old_presence")) (TC.runTCM . TC.out db)
		globalAndLocal cheoJid from ((resource from, T.unpack . bareTxt <$> jid):)
	go (RecordJoin cheoJid from jid) =
		globalAndLocal cheoJid from ((resource from, T.unpack . bareTxt <$> jid):)
	go (RecordPart cheoJid from) = do
		globalAndLocal cheoJid from (filter ((/=resource from) . fst))
	go (RecordNickChanged cheoJid from nick) =
		globalAndLocal cheoJid from $
			map (first $ \n -> if fromString n == resource from then T.unpack nick else n)
	go (Clear cheoJid from) =
		forM_ (tcKey cheoJid (muc from <> "\0presence")) (TC.runTCM . TC.out db)
	go (StartRejoin cheoJid from) = do
		-- Copy current presences to a holding space so we can clear when rejoin is over
		presences <- (fromMaybe [] . (readZ =<<)) <$>
			maybe (return Nothing) (TC.runTCM . TC.get db) (tcKey cheoJid (muc from <> "\0presence"))
		old_presences <- (fromMaybe [] . (readZ =<<)) <$>
			maybe (return Nothing) (TC.runTCM . TC.get db) (tcKey cheoJid (muc from <> "\0old_presence"))
		tcPut db cheoJid (muc from <> "\0old_presence")
			(show (presences <> old_presences :: [(String, Maybe String)]))
		forM_ (tcKey cheoJid (muc from <> "\0presence")) (TC.runTCM . TC.out db)
	go (GetRoomPresences cheoJid from rtrn) = do
		presences <- (fromMaybe [] . (readZ =<<)) <$>
			maybe (return Nothing) (TC.runTCM . TC.get db) (tcKey cheoJid (muc from <> "\0presence"))
		old_presences <- (fromMaybe [] . (readZ =<<)) <$>
			maybe (return Nothing) (TC.runTCM . TC.get db) (tcKey cheoJid (muc from <> "\0old_presence"))
		atomically $ putTMVar rtrn $ sort $ nubBy (equating fst) $ presences <> old_presences

	globalAndLocal cheoJid from f = do
		modify ("presence\0" <> muc from) f
		forM_ (tcKey cheoJid (muc from <> "\0presence")) (\k -> modify k f)
	modify :: String -> ([(String, Maybe String)] -> [(String, Maybe String)]) -> IO ()
	modify k f = do
		presence <- fmap (fromMaybe [] . (readZ =<<)) (TC.runTCM $ TC.get db k)
		True <- TC.runTCM $ TC.put db k $ show $ sort $ nubBy (equating fst) $ f presence
		return ()
	muc = T.unpack . bareTxt
	resource x = fromMaybe "" (T.unpack . strResource <$> jidResource x)

data JoinPartDebounce = DebounceJoin JID JID (Maybe JID) | DebouncePart JID JID | DebounceExpire JID JID UTCTime deriving (Show)

joinPartDebouncer db backendHost sendToComponent componentJid toRoomPresences toJoinPartDebouncer = next mempty
	where
	next state = do
		msg <- atomically (readTChan toJoinPartDebouncer)
		go state msg >>= next

	recordJoinPart cheoJid from mjid join
		| join = atomically $ writeTChan toRoomPresences $ RecordJoin cheoJid from mjid
		| otherwise = atomically $ writeTChan toRoomPresences $ RecordPart cheoJid from

	sendPart cheoJid from time = forM_ (mapToBackend backendHost cheoJid) $ \smsJid -> do
		atomically $ writeTChan toRoomPresences $ RecordPart cheoJid from
		now <- getCurrentTime
		sendToComponent $ mkStanzaRec $ mkSMS componentJid smsJid $ mconcat [
				fromString "* ",
				fromMaybe mempty (strResource <$> jidResource from),
				fromString " left the group ",
				fromString $ show $ round ((now `diffUTCTime` time) / 60),
				fromString " minutes ago"
			]

	sendJoin cheoJid from time mjid = forM_ (mapToBackend backendHost cheoJid) $ \smsJid -> do
		let nick = fromMaybe mempty (strResource <$> jidResource from)
		presences <- syncCall toRoomPresences $ GetRoomPresences cheoJid from
		now <- getCurrentTime
		when (isNothing $ lookup (T.unpack nick) presences) $ do
			atomically $ writeTChan toRoomPresences $ RecordJoin cheoJid from mjid
			sendToComponent $ mkStanzaRec $ mkSMS componentJid smsJid $ mconcat [
					fromString "* ",
					nick,
					fromString " joined the group ",
					fromString $ show $ round ((now `diffUTCTime` time) / 60),
					fromString " minutes ago"
				]

	debounceCheck state cheoJid from mjid join =
		case Map.lookup (cheoJid, from) state of
			Just (_, _, j) | j /= join -> return $! Map.delete (cheoJid, from) state -- debounce
			Just (_, _, _) -> return state -- ignore dupe
			Nothing -> do
				expire <- fmap (fromMaybe (-1) . (readZ =<<)) (maybe (return Nothing) (TC.runTCM . TC.get db) (tcKey cheoJid "debounce"))
				time <- getCurrentTime
				if expire < 0 then recordJoinPart cheoJid from mjid join else
					void $ forkIO $ threadDelay (expire*1000000) >> atomically (writeTChan toJoinPartDebouncer $ DebounceExpire cheoJid from time)
				return $! Map.insert (cheoJid, from) (time, mjid, join) state

	go state (DebounceJoin cheoJid from mjid) =
		debounceCheck state cheoJid from mjid True
	go state (DebouncePart cheoJid from) =
		debounceCheck state cheoJid from Nothing False
	go state (DebounceExpire cheoJid from time) =
		case Map.updateLookupWithKey (\_ (t,m,j) -> if t == time then Nothing else Just (t,m,j)) (cheoJid, from) state of
			(Just (t, mjid, join), state')
				| t == time && join -> sendJoin cheoJid from time mjid >> return state'
				| t == time -> sendPart cheoJid from time >> return state'
			(_, state') -> return state'


adhocBotManager :: (UIO.Unexceptional m, TC.TCDB db) => db -> JID -> (XMPP.Message -> UIO.UIO ()) -> (XMPP.IQ -> UIO.UIO (STM (Maybe XMPP.IQ))) -> (STM XMPP.Message) -> m ()
adhocBotManager db componentJid sendMessage sendIQ messages = do
	cleanupChan <- atomicUIO newTChan
	statefulManager cleanupChan Map.empty
	where
	statefulManager cleanupChan sessions = do
		join $ atomicUIO $ (processMessage cleanupChan sessions <$> messages) <|> (cleanupSession cleanupChan sessions <$> readTChan cleanupChan)

	cleanupSession cleanupChan sessions sessionToClean = statefulManager cleanupChan $! (Map.delete sessionToClean sessions)

	processMessage cleanupChan sessions message = do
		-- XXX: At some point this should not include resource, but it makes it easy to test for now
		let key = bareTxt <$> (XMPP.stanzaFrom message)
		sessions' <- case Map.lookup key sessions of
			Just input -> input message >> return sessions
			Nothing -> do
				newChan <- atomicUIO newTChan
				UIO.forkFinally (adhocBotSession db componentJid sendMessage sendIQ (readTChan newChan) message) (\_ -> atomicUIO $ writeTChan cleanupChan key)
				let writer = (atomicUIO . writeTChan newChan)
				return $ Map.insert key writer sessions
		statefulManager cleanupChan sessions'

openTokyoCabinet :: (TC.TCDB a) => String -> IO a
openTokyoCabinet pth = TC.runTCM $ do
	db <- TC.new
	True <- TC.open db pth [TC.OREADER, TC.OWRITER, TC.OCREAT]
	return db

data Avatar = Avatar Text Int64 Text

mkAvatar :: FilePath -> IO Avatar
mkAvatar path = do
	png <- LZ.readFile path
	return $! Avatar
		(T.pack $ showDigest $ sha1 png)
		(LZ.length png)
		(decodeUtf8 $ Base64.encode $ LZ.toStrict png)

avatarMetadata :: Avatar -> XML.Element
avatarMetadata (Avatar hash size _) =
	XML.Element (s"{http://jabber.org/protocol/pubsub#event}event") [] [
		XML.NodeElement $ XML.Element (s"{http://jabber.org/protocol/pubsub#event}items")
		[(s"node", [XML.ContentText $ s"urn:xmpp:avatar:metadata"])] [
			XML.NodeElement $ XML.Element (s"{http://jabber.org/protocol/pubsub#event}item")
			[(s"id", [XML.ContentText hash])] [
				XML.NodeElement $ XML.Element (s"{urn:xmpp:avatar:metadata}metadata") [] [
					XML.NodeElement $ XML.Element (s"{urn:xmpp:avatar:metadata}info") [
						(s"id", [XML.ContentText hash]),
						(s"bytes", [XML.ContentText $ tshow size]),
						(s"type", [XML.ContentText $ s"image/png"])
					] []
				]
			]
		]
	]

main :: IO ()
main = do
	hSetBuffering stdout LineBuffering
	hSetBuffering stderr LineBuffering

	args <- getArgs
	case args of
		("register":componentHost:host:port:secret:backendHost:did:password:[]) -> do
			log "" "Registering..."
			let Just componentJid = parseJID (fromString componentHost)
			let Just gatewayJid = parseJID (fromString backendHost)
			void $ runComponent (Server componentJid host (PortNumber $ read port)) (fromString secret) $ do
				mapM_ putStanza =<< registerToGateway componentJid gatewayJid (fromString did) (fromString password)
				liftIO $ threadDelay 1000000
		[config] -> do
			(Config.Config componentJid (Config.ServerConfig host port) secret backendHost rawdid registrationJid conferences s5bListenOn (Config.ServerConfig s5bhost s5bport) jingleStore jingleStoreURL redisConnectInfo (Config.ServerConfig statsdHost statsdPort) maybeAvatarPath) <- Dhall.input Dhall.auto (fromString config)
			log "" "Starting..."
			let Just did = normalizeTel rawdid
			db <- openTokyoCabinet "./db.tcdb" :: IO TC.HDB
			redis <- Redis.checkedConnect redisConnectInfo
			toJoinPartDebouncer <- atomically newTChan
			sendToComponent <- atomically newTChan
			toStanzaProcessor <- atomically newTChan
			toRoomPresences <- atomically newTChan
			toRejoinManager <- atomically newTChan

			statsd <- openStatsD statsdHost (show statsdPort) ["cheogram"]

			(sendIQ, iqReceiver) <- iqManager $ atomicUIO . writeTChan sendToComponent . mkStanzaRec
			adhocBotMessages <- atomically newTChan
			void $ forkIO $ adhocBotManager db componentJid (atomicUIO . writeTChan sendToComponent . mkStanzaRec) sendIQ (readTChan adhocBotMessages)

			void $ forkIO $ joinPartDebouncer db backendHost (atomically . writeTChan sendToComponent) componentJid toRoomPresences toJoinPartDebouncer
			void $ forkIO $ roomPresences db toRoomPresences

			void $ forkIO $ forever $ atomically (writeTChan toRejoinManager CheckPings) >> threadDelay 120000000
			void $ forkIO $ rejoinManager db (atomically . writeTChan sendToComponent) (textToString $ formatJID componentJid) toRoomPresences toRejoinManager

			-- When we're talking to the adhoc bot we'll get a command from stuff\40example.com@cheogram.com
			-- When they're talking to us directly, we'll get the command from stuff@example.com
			-- In either case, we want to use the same key and understand it as coming from the same user
			let maybeUnescape userJid
				| jidDomain userJid == jidDomain componentJid,
				  Just node <- jidNode userJid =
					let resource = maybe mempty strResource $ jidResource userJid
					in
					-- If we can't parse the thing we unescaped, just return the original
					fromMaybe userJid $ parseJID (unescapeJid (strNode node) ++ if T.null resource then mempty else s"/" ++ resource)
				| otherwise = userJid

			processDirectMessageRouteConfig <- ConfigureDirectMessageRoute.main (XMPP.jidDomain componentJid)
				(\userJid ->
					let userJid' = maybeUnescape userJid in
					(parseJID . fromString =<<) <$> TC.runTCM (TC.get db (T.unpack (bareTxt userJid') ++ "\0possible-route"))
				)
				(\userJid ->
					let userJid' = maybeUnescape userJid in
					(parseJID . fromString =<<) <$> TC.runTCM (TC.get db (T.unpack (bareTxt userJid') ++ "\0direct-message-route"))
				)
				(\userJid mgatewayJid -> do
					let userJid' = maybeUnescape userJid
					TC.runTCM (TC.out db (T.unpack (bareTxt userJid') ++ "\0possible-route"))
					case mgatewayJid of
						Just gatewayJid -> do
							maybeExistingRoute <- (parseJID . fromString =<<) <$> TC.runTCM (TC.get db (T.unpack (bareTxt userJid') ++ "\0direct-message-route"))
							forM_ maybeExistingRoute $ \existingRoute ->
								when (existingRoute /= gatewayJid)
									(atomically . writeTChan sendToComponent . mkStanzaRec =<< unregisterDirectMessageRoute db componentJid userJid' existingRoute)

							True <- TC.runTCM $ TC.put db (T.unpack (bareTxt userJid') ++ "\0direct-message-route") (T.unpack $ formatJID gatewayJid)

							forM_ (parseJID $ escapeJid (bareTxt userJid') ++ s"@" ++ formatJID componentJid) $ \from ->
								forM_ (parseJID $ did ++ s"@" ++ formatJID gatewayJid) $ \to ->
									atomically $ writeTChan sendToComponent $ mkStanzaRec $
										mkSMS from to (s"/addjid " ++ bareTxt userJid')

							return ()
						Nothing -> do
							maybeExistingRoute <- (parseJID . fromString =<<) <$> TC.runTCM (TC.get db (T.unpack (bareTxt userJid') ++ "\0direct-message-route"))
							TC.runTCM $ TC.out db (T.unpack (bareTxt userJid') ++ "\0direct-message-route")
							forM_ maybeExistingRoute $ \existingRoute ->
								atomically . writeTChan sendToComponent . mkStanzaRec =<< unregisterDirectMessageRoute db componentJid userJid' existingRoute
				)

			jingleHandler <- UIO.runEitherIO $ Jingle.setupJingleHandlers jingleStore s5bListenOn (fromString s5bhost, s5bport)
				(log "JINGLE")
				(\iq@(IQ { iqPayload = Just jingle }) path ->
					forM_ (isNamed (s"{urn:xmpp:jingle:1}content") =<< elementChildren jingle) $ \content -> do
					let fileDesc = mfilter (/=mempty) $ fmap (mconcat . elementText) $ headZ (isNamed (s"{urn:xmpp:jingle:apps:file-transfer:5}desc") =<< elementChildren =<< isNamed (s"{urn:xmpp:jingle:apps:file-transfer:5}file") =<< elementChildren =<< isNamed (s"{urn:xmpp:jingle:apps:file-transfer:5}description") =<< elementChildren content)
					atomicUIO $ writeTChan toStanzaProcessor $
						let url = jingleStoreURL ++ (T.takeWhileEnd (/='/') $ fromString path) in
						ReceivedMessage $ (emptyMessage MessageNormal) {
							messageFrom = iqFrom iq,
							messageTo = iqTo iq,
							messagePayloads = [
								Element (s"{jabber:component:accept}body") [] [NodeContent $ ContentText $ maybe mempty (++s"\n") fileDesc ++ url],
								Element (s"{jabber:x:oob}x") [] ([
									NodeElement $ Element (s"{jabber:x:oob}url") [] [NodeContent $ ContentText url]
								] ++ (maybe [] (\desc -> pure $ NodeElement $ Element (s"{jabber:x:oob}desc") [] [NodeContent $ ContentText desc]) fileDesc))
							]
						}
					fromIO_ $ atomically $ writeTChan sendToComponent $ mkStanzaRec $ (emptyIQ IQSet) {
						iqTo = iqFrom iq,
						iqFrom = iqTo iq,
						iqPayload = Just $ Element
							(s"{urn:xmpp:jingle:1}jingle")
							[(s"action", [s"session-info"]), (s"sid", [ContentText $ fromMaybe mempty $ attributeText (s"sid") jingle])]
							[
								NodeElement $ Element (s"{urn:xmpp:jingle:apps:file-transfer:5}received")
								[(s"creator", fromMaybe [] $ attributeContent (s"creator") content), (s"name", fromMaybe [] $ attributeContent (s"name") content)] []
							]
						}
					fromIO_ $ atomically $ writeTChan sendToComponent $ mkStanzaRec $ (emptyIQ IQSet) {
						iqTo = iqFrom iq,
						iqFrom = iqTo iq,
						iqID = Just $ s"id-session-terminate",
						iqPayload = Just $ Element
							(s"{urn:xmpp:jingle:1}jingle")
							[(s"action", [s"session-terminate"]), (s"sid", [ContentText $ fromMaybe mempty $ attributeText (s"sid") jingle])]
							[
								NodeElement $ Element (s"{urn:xmpp:jingle:1}reason")
								[]
								[
									NodeElement $ Element (s"{urn:xmpp:jingle:1}success") [] []
								]
							]
						}
				) (\iq@(IQ { iqFrom = Just from, iqTo = Just to }) -> do
					maybeProxy <- fmap (join . hush) $ UIO.fromIO $ getSipProxy db componentJid sendIQ from
					fromIO_ $ atomically $ writeTChan sendToComponent $ mkStanzaRec $ case maybeProxy of
						Just proxy ->
							rewriteJingleInitiatorResponder $ iq {
								iqFrom = parseJID $ escapeJid (bareTxt from) ++ s"@" ++ formatJID componentJid ++ s"/CHEOGRAM%outbound-sip%" ++ fromMaybe mempty (strResource <$> jidResource from),
								iqTo = parseJID $ escapeJid (fromMaybe mempty (strNode <$> jidNode to) ++ s"@" ++ proxy) ++ s"@sip.cheogram.com/sip"
							}
						Nothing -> iqNotImplemented iq
				)

			let pushStatsd = void . UIO.fromIO . StatsD.push statsd
			maybeAvatar <- mapM mkAvatar maybeAvatarPath

			log "" "runComponent STARTING"

			log "runComponent ENDED" =<< runComponent (Server componentJid host (PortNumber port)) secret
				(component db redis (UIO.lift . pushStatsd) backendHost did maybeAvatar (cacheOOB (UIO.lift . pushStatsd) jingleStore jingleStoreURL) sendIQ iqReceiver (writeTChan adhocBotMessages) toRoomPresences toRejoinManager toJoinPartDebouncer sendToComponent toStanzaProcessor processDirectMessageRouteConfig jingleHandler componentJid [registrationJid] conferences)
		_ -> log "ERROR" "Bad arguments"
