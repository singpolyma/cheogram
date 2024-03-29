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
import System.Exit (die)
import Control.Error (readZ, MaybeT(..), hoistMaybe, headZ, justZ, hush, atZ)
import Data.Time (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Network.Socket (PortNumber)
import Network.URI (parseURI, uriPath, escapeURIString)
import System.Random (Random(randomR), getStdRandom)
import System.Random.Shuffle (shuffleM)
import Data.Digest.Pure.SHA (sha1, bytestringDigest, showDigest)
import Network.StatsD (openStatsD)
import qualified Network.StatsD as StatsD
import Magic (Magic, MagicFlag(MagicMimeType), magicOpen, magicLoadDefault, magicFile)
import Network.Mime (defaultMimeMap)

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
import qualified Data.Map.Strict as SMap
import qualified Data.UUID as UUID ( toString )
import qualified Data.UUID.V1 as UUID ( nextUUID )
import qualified Data.ByteString.Lazy as LZ
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Builder as Builder
import qualified Database.Redis as Redis
import qualified Text.Regex.PCRE.Light as PCRE
import qualified Network.Http.Client as HTTP
import qualified System.IO.Streams as Streams
import Network.Protocol.XMPP as XMPP -- should import qualified

import Util
import IQManager
import qualified ConfigureDirectMessageRoute
import qualified JidSwitch
import qualified Config
import qualified DB
import qualified VCard4
import Adhoc (adhocBotSession, commandList, queryCommandList)
import StanzaRec

instance Ord JID where
	compare x y = compare (show x) (show y)

-- Do not use uncommon file extensions for ambiguous MIME types
badExts :: [Text]
badExts = [
		s"mpg4", s"mp4v",
		s"mpga", s"m2a", s"m3a", s"mp2", s"mp2a",
		s"m1v", s"m2v", s"mpe"
	]

mimeToExtMap :: SMap.Map String Text
mimeToExtMap = SMap.fromList $
	(\xs -> ("audio/amr", s"amr") : ("audio/AMR", s"amr") : xs) $
	mapMaybe (\(ext, mimeBytes) ->
		if ext `elem` badExts then
			Nothing
		else
			Just (textToString (decodeUtf8 mimeBytes), ext)
	) $ SMap.toList defaultMimeMap

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

nickFor db componentJid jid existingRoom
	| fmap bareTxt existingRoom == Just bareFrom = return $ fromMaybe (s"nonick") resourceFrom
	| jidDomain componentJid == jidDomain jid,
	  Just tel <- mfilter isE164 (strNode <$> jidNode jid) = do
		mnick <- DB.get db (DB.byNode jid ["nick"])
		case mnick of
			Just nick -> return (tel <> s" \"" <> nick <> s"\"")
			Nothing -> return tel
	| otherwise = return bareFrom
	where
	bareFrom = bareTxt jid
	resourceFrom = strResource <$> jidResource jid

code str status =
	hasAttributeText (fromString "{http://jabber.org/protocol/muc#user}code") (== fromString str) status
	<>
	hasAttributeText (fromString "code") (== fromString str) status

-- When we're talking to the adhoc bot we'll get a command from stuff\40example.com@cheogram.com
-- When they're talking to us directly, we'll get the command from stuff@example.com
-- In either case, we want to use the same key and understand it as coming from the same user
maybeUnescape componentJid userJid
	| jidDomain userJid == jidDomain componentJid,
	  Just node <- jidNode userJid =
		let resource = maybe mempty strResource $ jidResource userJid
		in
		-- If we can't parse the thing we unescaped, just return the original
		fromMaybe userJid $ parseJID (unescapeJid (strNode node) ++ if T.null resource then mempty else s"/" ++ resource)
	| otherwise = userJid

cheogramDiscoInfo db componentJid sendIQ from q = do
	canVoice <- isJust <$> getSipProxy db componentJid sendIQ from
	return $ Element (s"{http://jabber.org/protocol/disco#info}query")
		(map (\node -> (s"{http://jabber.org/protocol/disco#info}node", [ContentText node])) $ maybeToList $ nodeAttribute =<< q)
		(catMaybes [
			Just $ NodeElement $ Element (s"{http://jabber.org/protocol/disco#info}identity") [
				(s"category", [ContentText $ s"gateway"]),
				(s"type", [ContentText $ s"sms"]),
				(s"name", [ContentText $ s"Cheogram"])
				] [],
			mfilter (const canVoice) $ Just $ NodeElement $ Element (s"{http://jabber.org/protocol/disco#info}identity") [
				(s"category", [ContentText $ s"gateway"]),
				(s"type", [ContentText $ s"pstn"]),
				(s"name", [ContentText $ s"Cheogram"])
			] [],
			Just $ NodeElement $ Element (s"{http://jabber.org/protocol/disco#info}feature") [
				(s"var", [ContentText $ s"http://jabber.org/protocol/commands"])
			] [],
			Just $ NodeElement $ Element (s"{http://jabber.org/protocol/disco#info}feature") [
				(s"var", [ContentText $ s"jabber:iq:gateway"])
			] [],
			Just $ NodeElement $ Element (s"{http://jabber.org/protocol/disco#info}feature") [
				(s"var", [ContentText $ s"jabber:iq:register"])
			] [],
			Just $ NodeElement $ Element (s"{http://jabber.org/protocol/disco#info}feature") [
				(s"var", [ContentText $ s"urn:xmpp:ping"])
			] [],
			Just $ NodeElement $ Element (s"{http://jabber.org/protocol/disco#info}feature") [
				(s"var", [ContentText $ s"vcard-temp"])
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

getSipProxy :: DB.DB -> JID -> (IQ -> UIO (STM (Maybe IQ))) -> JID -> IO (Maybe Text)
getSipProxy db componentJid sendIQ jid = do
	maybeProxy <- DB.get db (DB.byJid jid ["sip-proxy"])
	case maybeProxy of
		Just proxy -> return $ Just proxy
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
	maybeRoute <- DB.get db (DB.byJid from ["direct-message-route"])
	case (maybeRoute, maybeRouteFrom) of
		(Just route, Just routeFrom) ->
				let routeTo = fromMaybe componentJid $ parseJID $ (maybe mempty (++ s"@") $ strNode <$> jidNode smsJid) ++ route in
				query routeTo routeFrom
		_ -> return [mkStanzaRec $ reply]
	where
	maybeRouteFrom = parseJID $ escapeJid (bareTxt from) ++ s"@" ++ formatJID componentJid ++ s"/" ++ (fromString resource)

routeQueryStateful db componentJid sendIQ from targetNode query = hasLocked "routeQueryStateful" $ do
	maybeRoute <- DB.get db (DB.byJid from ["direct-message-route"])
	case (maybeRoute, maybeRouteFrom) of
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

deleteDirectMessageRoute db userJid = do
	DB.del db (DB.byJid userJid ["direct-message-route"])
	mcheoJid <- fmap (parseJID =<<) $ DB.get db (DB.byJid userJid ["cheoJid"])
	forM_ mcheoJid $ \cheoJid -> do
		DB.del db (DB.byJid userJid ["cheoJid"])
		DB.srem db (DB.byNode cheoJid ["owners"]) [bareTxt userJid]

unregisterDirectMessageRoute db componentJid userJid route = do
	maybeCheoJid <- (parseJID =<<) <$> DB.get db (DB.byJid userJid ["cheoJid"])
	forM_ maybeCheoJid $ \cheoJid -> do
		DB.del db (DB.byJid userJid ["cheoJid"])
		DB.srem db (DB.byNode cheoJid ["owners"]) [bareTxt userJid]

	uuid <- (fmap.fmap) (fromString . UUID.toString) UUID.nextUUID
	return $ (emptyIQ IQSet) {
			iqTo = Just route,
			iqFrom = parseJID $ escapeJid (bareTxt userJid) ++ s"@" ++ formatJID componentJid ++ s"/CHEOGRAM%removed",
			iqID = uuid,
			iqPayload = Just $ Element (s"{jabber:iq:register}query") [] [
				NodeElement $ Element (s"{jabber:iq:register}remove") [] []
			]
		}

toRouteOrFallback db componentJid from smsJid m fallback = do
	maybeRoute <- DB.get db (DB.byJid from ["direct-message-route"])
	case (maybeRoute, parseJID $ escapeJid (bareTxt from) ++ s"@" ++ formatJID componentJid ++ resourceSuffix) of
		(Just route, Just routeFrom) -> do
			return [mkStanzaRec $ m {
				messageFrom = Just routeFrom,
				messageTo = parseJID $ (fromMaybe mempty $ strNode <$> jidNode smsJid) ++ s"@" ++ route
			}]
		_ -> fallback
	where
	resourceSuffix = maybe mempty (s"/"++) (strResource <$> jidResource from)

componentMessage db componentJid (m@Message { messageType = MessageError }) _ from smsJid body = do
	log "MESSAGE ERROR"  m
	toRouteOrFallback db componentJid from smsJid m $ do
		log "DIRECT FROM GATEWAY" smsJid
		return [mkStanzaRec $ m { messageTo = Just smsJid, messageFrom = Just componentJid }]
componentMessage db componentJid m@(Message { messageTo = Just to@JID{ jidNode = Just _ } }) existingRoom _ smsJid _
	| Just invite <- getMediatedInvitation m <|> getDirectInvitation m = do
		forM_ (invitePassword invite) $ \password ->
			DB.set db (DB.byNode to [textToString $ formatJID $ inviteMUC invite, "muc_roomsecret"]) password
		existingInvite <- (parseJID =<<) <$> DB.get db (DB.byNode to ["invited"])
		nick <- nickFor db componentJid (inviteFrom invite) existingRoom
		let txt = mconcat [
				fromString "* ",
				nick,
				fromString " has invited you to a group",
				maybe mempty (\t -> fromString ", saying \"" <> t <> fromString "\"") (inviteText invite),
				fromString "\nYou can switch to this group by replying with /join"
			]
		if (existingRoom /= Just (inviteMUC invite) && existingInvite /= Just (inviteMUC invite)) then do
			DB.set db (DB.byNode to ["invited"]) (formatJID $ inviteMUC invite)
			regJid <- (parseJID =<<) <$> DB.get db (DB.byNode to ["registered"])
			fmap (((mkStanzaRec $ mkSMS componentJid smsJid txt):) . concat . toList)
				(forM regJid $ \jid -> sendInvite db jid (invite { inviteFrom = to }))
		else
			return []
componentMessage _ componentJid (m@Message { messageType = MessageGroupChat }) existingRoom from smsJid (Just body) = do
	if fmap bareTxt existingRoom == Just (bareTxt from) && (
	   existingRoom /= Just from ||
	   not (fromString "CHEOGRAM%" `T.isPrefixOf` fromMaybe mempty (messageID m))) then
		return [mkStanzaRec $ mkSMS componentJid smsJid txt]
	else do
		log "MESSAGE FROM WRONG GROUP" (fmap bareTxt existingRoom, from, m)
		return []
	where
	txt = mconcat [fromString "(", fromMaybe (fromString "nonick") (strResource <$> jidResource from), fromString ") ", body]
componentMessage db componentJid m@(Message { messageTo = Just to }) existingRoom from smsJid (Just body) = do
	ack <- case isNamed (fromString "{urn:xmpp:receipts}request") =<< messagePayloads m of
		(_:_) ->
			routeDiscoOrReply db componentJid from smsJid ("CHEOGRAM%query-then-send-ack%" ++ extra) Nothing
				(deliveryReceipt (fromMaybe mempty $ messageID m) to from)
		[] -> return []

	fmap (++ack) $ toRouteOrFallback db componentJid from smsJid strippedM $
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
				nick <- nickFor db componentJid from existingRoom
				let txt = mconcat [s"<", nick, s" says> ", strippedBody]
				let sms = mkSMS componentJid smsJid txt
				let thread = (maybe id (\t f -> f ++ s" " ++ t) (getThread "jabber:component:accept" m)) (bareTxt from)
				return [mkStanzaRec $ sms { messagePayloads = (Element (s"{jabber:component:accept}thread") [] [NodeContent $ ContentText thread]) : messagePayloads sms } ]
	where
	strippedM = mapBody (const strippedBody) m
	strippedBody = stripOtrWhitespace body
	extra = T.unpack $ escapeJid $ T.pack $ show (fromMaybe mempty (messageID m), maybe mempty strResource $ jidResource from)
componentMessage _ _ m _ _ _ _ = do
	log "UNKNOWN MESSAGE" m
	return []

handleJoinPartRoom db toRoomPresences toRejoinManager toJoinPartDebouncer componentJid existingRoom from to smsJid payloads join
	| join,
	  [x] <- isNamed (s"{http://jabber.org/protocol/muc#user}x") =<< payloads,
	  not $ null $ code "110" =<< isNamed (fromString "{http://jabber.org/protocol/muc#user}status") =<< elementChildren x = do
		existingInvite <- (parseJID =<<) <$> DB.get db (DB.byNode to ["invited"])
		when (existingInvite == parseJID bareMUC) $
			DB.del db (DB.byNode to ["invited"])
		DB.set db (DB.byNode to ["joined"]) (formatJID from)
		DB.sadd db (DB.byNode to ["bookmarks"]) [bareMUC]

		presences <- syncCall toRoomPresences $ GetRoomPresences to from
		atomically $ writeTChan toRoomPresences $ RecordSelfJoin to from (Just to)

		atomically $ writeTChan toRejoinManager $ Joined from

		case presences of
			[] -> do -- No one in the room, so we "created"
				uuid <- fromMaybe "UUIDFAIL" <$> (fmap.fmap) (fromString . UUID.toString) UUID.nextUUID
				let fullid = if (resourceFrom `elem` map fst presences) then uuid else "CHEOGRAMCREATE%" <> uuid
				return [mkStanzaRec $ (emptyIQ IQGet) {
					iqTo = Just room,
					iqFrom = Just to,
					iqID = Just $ fromString fullid,
					iqPayload = Just $ Element (fromString "{http://jabber.org/protocol/muc#owner}query") [] []
				}]
			(_:_) | isNothing (lookup resourceFrom presences) -> do
				fmap ((mkStanzaRec $ mkSMS componentJid smsJid $ mconcat [
						s"* You have joined ", bareMUC,
						s" as ", resourceFrom,
						s" along with\n",
						intercalate (s", ") (filter (/= resourceFrom) $ map fst presences)
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
		log "SERVER RESTART, clear join status" (to, from)
		void $ atomically (writeTChan toRejoinManager $ JoinError from)
		return []
	| not join && existingRoom == Just from = do
		DB.del db (DB.byNode to ["joined"])
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
	forM_ (iqFrom iq) $ \from ->
		DB.set db (DB.byJid from ["registration_code"]) $ tshow $ RegistrationCode code (formatJID to) time
	return [
			mkStanzaRec $ mkSMS componentJid to $ fromString ("Enter this verification code to complete registration: " <> show code),
			mkStanzaRec $ iq {
				iqTo = iqFrom iq,
				iqFrom = iqTo iq,
				iqType = IQResult,
				iqPayload = Just verificationResponse
			}
		]

handleVerificationCode db componentJid password iq from = do
	time <- getCurrentTime
	codeAndTime <- fmap (readZ . textToString =<<) $ DB.get db (DB.byJid from ["registration_code"])
	case codeAndTime of
		Just (RegistrationCode { regCode = code, cheoJid = cheoJidT })
			| fmap expires codeAndTime > Just ((-300) `addUTCTime` time) ->
				case (show code == T.unpack password, iqTo iq, parseJID cheoJidT) of
					(True, Just to, Just cheoJid) -> do
						bookmarks <- DB.smembers db (DB.byNode cheoJid ["bookmarks"])
						invites <- fmap concat $ forM (mapMaybe parseJID bookmarks) $ \bookmark ->
							sendInvite db from (Invite bookmark cheoJid (Just $ fromString "Cheogram registration") Nothing)

						let Just tel = strNode <$> jidNode cheoJid
						DB.set db (DB.byJid from ["registered"]) tel
						DB.set db (DB.byNode cheoJid ["registered"]) (bareTxt from)

						stuff <- runMaybeT $ do
							-- If there is a nick that doesn't end in _sms, add _sms
							nick <- MaybeT $ DB.get db (DB.byNode cheoJid ["nick"])
							let nick' = (fromMaybe nick $ T.stripSuffix (s"_sms") nick) <> s"_sms"
							liftIO $ DB.set db (DB.byNode cheoJid ["nick"]) nick'

							room <- MaybeT $ (parseJID =<<) <$> DB.get db (DB.byNode cheoJid ["joined"])
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
			DB.del db (DB.byJid from ["registration_code"])
			return []

handleRegister db componentJid iq@(IQ { iqType = IQGet, iqFrom = Just from }) _ = do
	time <- getCurrentTime
	codeAndTime <- fmap (readZ . textToString =<<) $ DB.get db (DB.byJid from ["registration_code"])
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
	  Just to <- (`telToJid` formatJID componentJid) =<< getFormField form (fromString "phone") = do
		registerVerification db componentJid to iq
handleRegister db componentJid iq@(IQ { iqType = IQSet }) query
	| [phoneEl] <- isNamed (fromString "{jabber:iq:register}phone") =<< elementChildren query,
	  Just to <- (`telToJid` formatJID componentJid) $ mconcat (elementText phoneEl) = do
		registerVerification db componentJid to iq
handleRegister db componentJid iq@(IQ { iqType = IQSet, iqFrom = Just from }) query
	| [form] <- isNamed (fromString "{jabber:x:data}x") =<< elementChildren query,
	  Just password <- getFormField form (fromString "password") = do
		handleVerificationCode db componentJid password iq from
handleRegister db componentJid iq@(IQ { iqType = IQSet, iqPayload = Just payload, iqFrom = Just from }) query
	| [passwordEl] <- isNamed (fromString "{jabber:iq:register}password") =<< elementChildren query = do
		handleVerificationCode db componentJid (mconcat $ elementText passwordEl) iq from
handleRegister db componentJid iq@(IQ { iqTo = Just to, iqFrom = Just from, iqType = IQSet }) query
	| [_] <- isNamed (fromString "{jabber:iq:register}remove") =<< elementChildren query = do
		tel <- fromMaybe mempty <$> DB.get db (DB.byJid from ["registered"])
		forM_ (telToJid tel (formatJID componentJid)) $ \cheoJid ->
			DB.del db (DB.byNode cheoJid ["registered"])
		DB.del db (DB.byJid from ["registered"])

		return [mkStanzaRec $
				iqReply
				(Just $ Element (fromString "{jabber:iq:register}query") [] [])
				iq
			]
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
	db :: DB.DB,
	pushStatsd :: [StatsD.Stat] -> IO (),
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
	| MessageError == messageType m = return []
	| Just _ <- getBody "jabber:component:accept" m = do
		hasLocked "adhocBotMessage" $ atomicUIO $ adhocBotMessage m
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
componentStanza (ComponentContext { db, smsJid = (Just smsJid), componentJid, ctxCacheOOB }) (ReceivedMessage (m@Message { messageTo = Just to@(JID { jidNode = Just _ }), messageFrom = Just from})) = do
	existingRoom <- (parseJID =<<) <$> DB.get db (DB.byNode to ["joined"])
	m' <- UIO.lift $ ctxCacheOOB m
	componentMessage db componentJid m' existingRoom from smsJid $
		getBody "jabber:component:accept" m'
componentStanza (ComponentContext { smsJid = (Just smsJid), toRejoinManager, componentJid }) (ReceivedPresence p@(Presence { presenceType = PresenceError, presenceFrom = Just from, presenceTo = Just to, presenceID = Just id }))
	| fromString "CHEOGRAMREJOIN%" `T.isPrefixOf` id = do
		log "FAILED TO REJOIN, clear join state" p
		void $ atomically (writeTChan toRejoinManager $ JoinError from)
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
		presenceTo = Just to@(JID { jidNode = Just _ }),
		presencePayloads = payloads
	})) | typ `elem` [PresenceAvailable, PresenceUnavailable] = do
		existingRoom <- (parseJID =<<) <$> DB.get db (DB.byNode to ["joined"])
		hasLocked "handleJoinPartRoom" $ handleJoinPartRoom db toRoomPresences toRejoinManager toJoinPartDebouncer componentJid existingRoom from to smsJid payloads (typ == PresenceAvailable)
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
	  item <- headZ $ isNamed (s"{http://jabber.org/protocol/pubsub}item") =<<
		elementChildren items,
	  isNothing item || (attributeText (s"id") =<< item) == Just hash =
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
			let subscribe = if (attributeText (s"status") =<< iqPayload replyIQ) /= Just (s"completed") then [] else [
					mkStanzaRec $ (emptyPresence PresenceSubscribe) {
						presenceTo = iqFrom iq,
						presenceFrom = Just componentJid,
						presencePayloads = [
							Element (s"{jabber:component:accept}status") [] [
								NodeContent $ ContentText $ s"Add this contact and then you can SMS by sending messages to +1<phone-number>@" ++ formatJID componentJid ++ s" Jabber IDs."
							]
						]
					}
				]

			let fromLocalpart = maybe mempty (\localpart -> localpart++s"@") (fmap strNode . jidNode =<< iqFrom replyIQ)
			return $ subscribe ++ [mkStanzaRec $ replyIQ {
				iqFrom = parseJID (fromLocalpart ++ formatJID componentJid ++ s"/CHEOGRAM%" ++ ConfigureDirectMessageRoute.nodeName)
			}]
componentStanza (ComponentContext { db, componentJid }) (ReceivedIQ iq@(IQ { iqTo = Just (JID { jidNode = Nothing }), iqPayload = payload, iqFrom = Just from }))
	| fmap elementName payload == Just (s"{http://jabber.org/protocol/commands}command") && (attributeText (s"node") =<< payload) == Just JidSwitch.nodeName =
		let setJidSwitch newJid = do
			let from' = maybeUnescape componentJid from
			Just route <- (XMPP.parseJID <=< id) <$> DB.get db (DB.byJid from' ["direct-message-route"])
			let key = DB.byJid newJid ["jidSwitch"]
			DB.hset db key $ JidSwitch.toAssoc from' route
			-- I figure 24 hours is a wide enough window to accept a JID switch
			DB.expire db key $ 60 * 60 * 24
			return (from', newJid, route)
		in
		map mkStanzaRec <$> JidSwitch.receiveIq componentJid setJidSwitch iq
componentStanza (ComponentContext { db, componentJid }) (ReceivedIQ iq@(IQ { iqTo = Just to, iqPayload = Just payload, iqFrom = Just from }))
	| jidNode to == Nothing,
	  elementName payload == s"{http://jabber.org/protocol/commands}command",
	  attributeText (s"node") payload == Just (s"sip-proxy-set"),
	  [form] <- isNamed (fromString "{jabber:x:data}x") =<< elementChildren payload,
	  Just proxy <- getFormField form (s"sip-proxy") = do
		if T.null proxy then
			DB.del db (DB.byJid from ["sip-proxy"])
		else
			DB.set db (DB.byJid from ["sip-proxy"]) proxy
		return [mkStanzaRec $ iqReply Nothing iq]
componentStanza (ComponentContext { db, componentJid }) (ReceivedIQ iq@(IQ { iqTo = Just to, iqPayload = Just payload, iqFrom = Just from }))
	| jidNode to == Nothing,
	  jidNode from == Nothing,
	  elementName payload == s"{http://jabber.org/protocol/commands}command",
	  attributeText (s"node") payload == Just (s"push-register"),
	  [form] <- isNamed (fromString "{jabber:x:data}x") =<< elementChildren payload,
	  Just pushRegisterTo <- XMPP.parseJID =<< getFormField form (s"to") = do
		DB.set db (DB.byJid pushRegisterTo ["possible-route"]) (XMPP.formatJID from)
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
				mkStanzaRec $ (XMPP.emptyMessage XMPP.MessageChat) {
					XMPP.messageTo = Just pushRegisterTo,
					XMPP.messageFrom = Just componentJid,
					XMPP.messagePayloads = [
						mkElement (s"{jabber:component:accept}body") $
							s"To start registration with " ++ XMPP.formatJID from ++ s" reply with: register " ++ XMPP.formatJID from ++
							s"\n(If you do not wish to start this registration, simply ignore this message.)",
						Element (s"{http://jabber.org/protocol/disco#items}query")
							[(s"node", [ContentText $ s"http://jabber.org/protocol/commands"])] [
								NodeElement $ Element (s"{http://jabber.org/protocol/disco#items}item") [
									(s"jid", [ContentText $ formatJID componentJid ++ s"/CHEOGRAM%" ++ ConfigureDirectMessageRoute.nodeName]),
									(s"node", [ContentText ConfigureDirectMessageRoute.nodeName]),
									(s"name", [ContentText $ s"register " ++ XMPP.formatJID from])
								] []
							]
					]
				}
			]
componentStanza (ComponentContext { db, componentJid }) (ReceivedIQ iq@(IQ { iqFrom = Just _, iqTo = Just (JID { jidNode = Nothing }), iqPayload = Just p }))
	| iqType iq `elem` [IQGet, IQSet],
	  [query] <- isNamed (fromString "{jabber:iq:register}query") p = do
		handleRegister db componentJid iq query
componentStanza (ComponentContext { db, pushStatsd, componentJid, maybeAvatar, sendIQ }) (ReceivedIQ (IQ { iqType = IQGet, iqFrom = Just from, iqTo = Just to, iqID = id, iqPayload = Just p }))
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

		pushStatsd [StatsD.stat ["cmd-list", "fetch"] 1 "c" Nothing]

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
		case telToJid (mconcat $ elementText prompt) (formatJID componentJid) of
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
				mnick <- DB.get db (DB.byNode cheoJid ["nick"])
				let nick = fromMaybe (maybe mempty strNode (jidNode cheoJid)) mnick
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
		if s"muc_membersonly" `elem` vars then
			DB.setEnum db (DB.byJid from ["muc_membersonly"]) True
		else
			DB.del db (DB.byJid from ["muc_membersonly"])
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
	maybeRoute <- DB.get db (DB.byJid from ["direct-message-route"])
	case (maybeRoute, parseJID $ escapeJid (bareTxt from) ++ s"@" ++ formatJID componentJid ++ resourceSuffix) of
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
			(reverse $ take 240 $ reverse $ escapeURIString isAlphaNum (textToString url))
			(hush <$> UIO.fromIO (fromMaybe mempty <$> Streams.read body))
		else
			return $ Left $ userError "Response was not 200 OK"

cacheOneOOB :: (Unexceptional m) => Magic -> ([StatsD.Stat] -> m ()) -> FilePath -> Text -> XML.Element -> m (Maybe (Text, Text), XML.Element)
cacheOneOOB magic pushStatsd jingleStore jingleStoreURL oob
	| [url] <- (mconcat . XML.elementText) <$> urls = do
		cacheResult <- cacheHTTP jingleStore url
		case cacheResult of
			Left err -> do
				pushStatsd [StatsD.stat ["cache", "oob", "failure"] 1 "c" Nothing]
				log "cacheOneOOB" err
				return (Nothing, oob)
			Right path -> do
				pushStatsd [StatsD.stat ["cache", "oob", "success"] 1 "c" Nothing]
				mimeType <- fromIO_ $ magicFile magic path
				let extSuffix = maybe mempty (s"." ++) $ SMap.lookup mimeType mimeToExtMap
				let url' = jingleStoreURL ++ (T.takeWhileEnd (/='/') $ fromString path) ++ extSuffix
				return (
						Just (url, url'),
						oob {
							XML.elementNodes =
								map XML.NodeElement
								(mkElement urlName url' : rest)
						}
					)
	| otherwise = do
		pushStatsd [StatsD.stat ["cache", "oob", "malformed"] 1 "c" Nothing]
		log "cacheOneOOB MALFORMED" oob
		return (Nothing, oob)
	where
	urlName = s"{jabber:x:oob}url"
	(urls, rest) = partition (\el -> XML.elementName el == urlName) (elementChildren oob)

cacheOOB :: (Unexceptional m) => Magic -> ([StatsD.Stat] -> m ()) -> FilePath -> Text -> XMPP.Message -> m XMPP.Message
cacheOOB magic pushStatsd jingleStore jingleStoreURL m@(XMPP.Message { XMPP.messagePayloads = payloads }) = do
	(replacements, oobs') <- unzip <$> mapM (cacheOneOOB magic pushStatsd jingleStore jingleStoreURL) oobs
	let body' =
		(mkElement bodyName .: foldl (\body (a, b) -> T.replace a b body)) <$>
		(map (mconcat . XML.elementText) body) <*> pure (catMaybes replacements)
	return $ m { XMPP.messagePayloads = noOobsNoBody ++ oobs' ++ body' }
	where
	oobName = s"{jabber:x:oob}x"
	bodyName = s"{jabber:component:accept}body"
	(body, noOobsNoBody) = partition (\el -> XML.elementName el == bodyName) noOobs
	(oobs, noOobs) = partition (\el -> XML.elementName el == oobName) payloads

component :: DB.DB
                     -> Redis.Connection
                     -> ([StatsD.Stat] -> UIO ())
                     -> Text
                     -> Text
                     -> Maybe Avatar
                     -> (Message -> UIO Message)
                     -> (IQ -> UIO (STM (Maybe IQ)))
                     -> (IQ -> XMPP ())
                     -> (Message -> STM ())
                     -> TChan RoomPresences
                     -> TChan RejoinManagerCommand
                     -> TChan JoinPartDebounce
                     -> TChan StanzaRec
                     -> TChan ReceivedStanza
                     -> (IQ -> IO (Maybe IQ))
                     -> (IQ -> XMPP ())
                     -> JID
                     -> [JID]
                     -> [Text]
                     -> XMPP ()
component db redis pushStatsd backendHost did maybeAvatar cacheOOB sendIQ iqReceiver adhocBotMessage toRoomPresences toRejoinManager toJoinPartDebouncer toComponent toStanzaProcessor processDirectMessageRouteConfig jingleHandler componentJid registrationJids conferenceServers = do
	sendThread <- forkXMPP $ forever $ flip catchError (log "component EXCEPTION") $ do
		stanza <- liftIO $ hasLocked "read toComponent" $ atomically $ readTChan toComponent

		UIO.lift $ pushStatsd [StatsD.stat ["stanzas", "out"] 1 "c" Nothing]
		putStanza =<< (liftIO . ensureId) stanza

	recvThread <- forkXMPP $ forever $ flip catchError (\e -> do
		log "component read EXCEPTION" e
		liftIO $ case e of
			TransportError _ -> die "Component connection gone"
			InvalidStanza el | not $ null $ isNamed (s"{http://etherx.jabber.org/streams}error") el -> die "Component connection error"
			_ -> return ()
		) $
		(liftIO . hasLocked "write toStanzaProcessor" . atomicUIO . writeTChan toStanzaProcessor) =<< getStanza

	flip catchError (\e -> liftIO (log "component part 2 EXCEPTION" e >> killThread sendThread >> killThread recvThread)) $ forever $ do
		stanza <- liftIO $ hasLocked "read toStanzaProcessor" $ atomicUIO $ readTChan toStanzaProcessor
		UIO.lift $ pushStatsd [StatsD.stat ["stanzas", "in"] 1 "c" Nothing]
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
					liftIO (mapM_ sendToComponent =<< processSMS db componentJid conferenceServers from cheoJid txt (getThread "jabber:component:accept" m))
			(Just from, Just to, Nothing, Just localpart, ReceivedMessage m)
				| Just txt <- getBody "jabber:component:accept" m,
				  Just owner <- parseJID (unescapeJid localpart),
				  (T.length txt == 144 || T.length txt == 145) && (s"CHEOGRAM") `T.isPrefixOf` txt -> liftIO $ do -- the length of our token messages
					log "POSSIBLE TOKEN" (from, to, txt)
					maybeRoute <- DB.get db (DB.byJid owner ["direct-message-route"])
					when (Just (strDomain $ jidDomain from) == maybeRoute || bareTxt from == bareTxt owner) $ do
						maybeToken <- DB.get db (DB.byJid owner ["addtoken"])
						case (fmap (first parseJID) (readZ . textToString =<< maybeToken)) of
							(Just (Just cheoJid, token)) | (s"CHEOGRAM"++token) == txt -> do
								log "SET OWNER" (cheoJid, owner)

								DB.set db (DB.byJid owner ["cheoJid"]) (formatJID cheoJid)
								DB.sadd db (DB.byNode cheoJid ["owners"]) [bareTxt owner]

							_ -> log "NO TOKEN FOUND, or mismatch" maybeToken
			(Just from, Just to, Nothing, Just localpart, _)
				| Just multipleTo <- mapM localpartToURI (T.split (==',') localpart),
				  ReceivedMessage m <- stanza,
				  Just backendJid <- parseJID backendHost -> liftIO $ do
					m' <- UIO.lift $ cacheOOB $ m { messagePayloads = messagePayloads m ++ [
							Element (s"{http://jabber.org/protocol/address}addresses") [] $ map (\oneto ->
								NodeElement $ Element (s"{http://jabber.org/protocol/address}address") [
									(s"{http://jabber.org/protocol/address}type", [ContentText $ s"to"]),
									(s"{http://jabber.org/protocol/address}uri", [ContentText oneto])
								] []
							) multipleTo
						] }
					-- TODO: should check if backend supports XEP-0033
					-- TODO: fallback for no-backend case should work
					mapM_ sendToComponent =<< componentMessage db componentJid m' Nothing from backendJid (getBody "jabber:component:accept" m')
				| (s"sip.cheogram.com") == strDomain (jidDomain from) -> liftIO $ do
					let (toResource, fromResource)
						| Just toResource <- T.stripPrefix (s"CHEOGRAM%outbound-sip%") =<< (strResource <$> jidResource to) = (toResource, s"tel")
						| otherwise = (fromMaybe mempty (strResource <$> jidResource to), s"sip:" ++ escapeJid (formatJID from))
					case (mapLocalpartToBackend (formatJID componentJid) =<< sanitizeSipLocalpart (maybe mempty strNode $ jidNode from), parseJID (unescapeJid localpart ++ s"/" ++ toResource)) of
						(Just componentFrom, Just routeTo) -> liftIO $ do
							Just componentFromSip <- return $ parseJID (formatJID componentFrom ++ s"/" ++ fromResource)
							sendToComponent $ mkStanzaRec $ receivedStanza $ receivedStanzaFromTo componentFromSip routeTo stanza
						_ ->
							mapM_ sendToComponent $ stanzaError stanza $
								Element (fromString "{jabber:component:accept}error")
								[(fromString "{jabber:component:accept}type", [ContentText $ fromString "cancel"])]
								[NodeElement $ Element (fromString "{urn:ietf:params:xml:ns:xmpp-stanzas}item-not-found") [] []]
			(Just from, Just to, Nothing, Just localpart, _)
				| Nothing <- mapM localpartToURI (T.split (==',') $ fromMaybe mempty $ fmap strNode $ jidNode to),
				  Just routeTo <- parseJID (unescapeJid localpart ++ maybe mempty (s"/"++) (strResource <$> jidResource to)),
				  fmap (((s"CHEOGRAM%") `T.isPrefixOf`) . strResource) (jidResource to) /= Just True -> liftIO $ do
					maybeRoute <- DB.get db (DB.byJid routeTo ["direct-message-route"])
					case (maybeRoute, mapToComponent from) of
						(Just route, _)
							| route == bareTxt from,
							  ReceivedIQ iq <- stanza,
							  iqType iq == IQSet,
							  [query] <- isNamed (fromString "{jabber:iq:register}query") =<< maybeToList (iqPayload iq) -> do
								deleteDirectMessageRoute db routeTo
								sendToComponent $ mkStanzaRec $ iqReply
									(Just $ Element (fromString "{jabber:iq:register}query") [] []) iq
						(Just route, Just componentFrom)
							| route == strDomain (jidDomain from),
							  ReceivedIQ iq <- stanza,
							  [items] <- XML.isNamed (s"{http://jabber.org/protocol/pubsub}items") =<< XML.elementChildren =<< XML.isNamed (s"{http://jabber.org/protocol/pubsub}pubsub") =<< justZ (iqPayload iq),
							  Just (s"urn:xmpp:vcard4") == XML.attributeText (s"node") items -> do
								mvcard4 <- UIO.lift $ VCard4.fetch sendIQ routeTo (Just componentFrom)
								case mvcard4 of
									Just vcard4 -> sendToComponent $ mkStanzaRec $ iqReply (
											Just $ XML.Element (s"{http://jabber.org/protocol/pubsub}pubsub") [] [
											XML.NodeElement $ XML.Element (s"{http://jabber.org/protocol/pubsub}items") [] [
											XML.NodeElement $ XML.Element (s"{http://jabber.org/protocol/pubsub}item") [] [XML.NodeElement vcard4]]]
										) iq
									Nothing -> sendToComponent $ mkStanzaRec $ iqNotImplemented iq
							| route == strDomain (jidDomain from) ->
								(sendToComponent . receivedStanza) =<< mapReceivedMessageM (UIO.lift . cacheOOB) (receivedStanzaFromTo componentFrom routeTo stanza)
						(Just route, _) -- Alphanumeric senders
							| route == strDomain (jidDomain from),
							  Just localpart <- strNode <$> jidNode from,
							  Nothing <- T.find (\c -> not ((isAlphaNum c || c == ' ') && isAscii c)) localpart ->
								let
									localpart' = T.concatMap (\c -> tshow (ord c - 30)) localpart ++ s";phone-context=alphanumeric.phone-context.soprani.ca"
									Just componentFrom = parseJID (localpart' ++ s"@" ++ formatJID componentJid)
								in
								(sendToComponent . receivedStanza) =<< mapReceivedMessageM (fmap (addNickname localpart) . UIO.lift . cacheOOB) (receivedStanzaFromTo componentFrom routeTo stanza)
						_ | Just jid <- (`telToJid` formatJID componentJid) =<< strNode <$> jidNode to -> do
							mapM_ sendToComponent $ stanzaError stanza $
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
							mapM_ sendToComponent $ stanzaError stanza $
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
					mapM_ sendToComponent =<< componentStanza (ComponentContext db (UIO.lift . pushStatsd) backendTo registrationJids adhocBotMessage cacheOOB toRoomPresences toRejoinManager toJoinPartDebouncer processDirectMessageRouteConfig componentJid sendIQ maybeAvatar) stanza
	where
	mapToComponent = mapToBackend (formatJID componentJid)
	sendToComponent = hasLocked "sendToComponent" . atomically . writeTChan toComponent

	stanzaError (ReceivedMessage m) errorPayload | messageType m /= MessageError =
		Just $ mkStanzaRec $ m {
			messageFrom = messageTo m,
			messageTo = messageFrom m,
			messageType = MessageError,
			messagePayloads = messagePayloads m ++ [errorPayload]
		}
	stanzaError (ReceivedPresence p) errorPayload | presenceType p /= PresenceError =
		Just $ mkStanzaRec $ p {
			presenceFrom = presenceTo p,
			presenceTo = presenceFrom p,
			presenceType = PresenceError,
			presencePayloads = presencePayloads p ++ [errorPayload]
		}
	stanzaError (ReceivedIQ iq) errorPayload | iqType iq /= IQError =
		Just $ mkStanzaRec $ iq {
			iqFrom = iqTo iq,
			iqTo = iqFrom iq,
			iqType = IQError,
			iqPayload = Just errorPayload
		}
	stanzaError _ _ = Nothing

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

parseCommand txt thread room nick componentJid
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
   | Just to <- parseJID =<< fmap (head . T.split (==' ')) thread =
		Just $ Whisper (maybeStripProxy to) txt
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

leaveRoom :: DB.DB -> JID -> String -> IO [StanzaRec]
leaveRoom db cheoJid@(JID { jidNode = Just _ }) reason = do
	existingRoom <- (parseJID =<<) <$> DB.get db (DB.byNode cheoJid ["joined"])
	return $ (flip map) (toList existingRoom) $ \leaveRoom ->
		mkStanzaRec $ (emptyPresence PresenceUnavailable) {
			presenceTo = Just leaveRoom,
			presenceFrom = Just cheoJid,
			presencePayloads = [Element (fromString "{jabber:component:accept}status") [] [NodeContent $ ContentText $ fromString reason]]
		}
leaveRoom _ cheoJid reason = do
	log "ERROR leaveRoom" (cheoJid, reason)
	return []

joinRoom db cheoJid room =
	rejoinRoom db cheoJid room False

rejoinRoom db cheoJid@(JID { jidNode = Just _ }) room rejoin = do
	password <- DB.get db (DB.byNode cheoJid [textToString (bareTxt room), "muc_roomsecret"])
	let pwEl = maybe [] (\pw -> [
			NodeElement $ Element (s"{http://jabber.org/protocol/muc}password") [] [NodeContent $ ContentText pw]
		]) password

	uuid <- fromMaybe "UUIDFAIL" <$> (fmap.fmap) (fromString . UUID.toString) UUID.nextUUID
	return [mkStanzaRec $ (emptyPresence PresenceAvailable) {
		presenceID = Just $ fromString $ (if rejoin then "CHEOGRAMREJOIN%" else "CHEOGRAMJOIN%") <> uuid,
		presenceTo = Just room,
		presenceFrom = Just cheoJid,
		presencePayloads = [Element (s"{http://jabber.org/protocol/muc}x") [] ([
			NodeElement $ Element (s"{http://jabber.org/protocol/muc}history") [(s"{http://jabber.org/protocol/muc}maxchars", [ContentText $ fromString "0"])] []
		] <> pwEl)]
	}]
rejoinRoom _ cheoJid room rejoin = do
	log "ERROR rejoinRoom" (cheoJid, room, rejoin)
	return []

addMUCOwner room from jid = do
	uuid <- (fmap.fmap) UUID.toString UUID.nextUUID
	return [mkStanzaRec $ (emptyIQ IQSet) {
		iqTo = Just room,
		iqFrom = Just from,
		iqID = fmap fromString uuid,
		iqPayload = Just $ Element (s"{http://jabber.org/protocol/muc#admin}admin") [] [
			NodeElement $ Element (s"{http://jabber.org/protocol/muc#admin}item")
				[
					(s"{http://jabber.org/protocol/muc#admin}affiliation", [ContentText $ s"owner"]),
					(s"{http://jabber.org/protocol/muc#admin}jid", [ContentText $ formatJID jid])
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
	membersonly <- fromMaybe False <$> DB.getEnum db (DB.byJid room ["muc_membersonly"])
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

processSMS db componentJid conferenceServers smsJid cheoJid txt thread = do
	nick <- fromMaybe (maybe (formatJID cheoJid) strNode (jidNode cheoJid)) <$> DB.get db (DB.byNode cheoJid ["nick"])
	existingRoom <- (fmap (\jid -> jid { jidResource = Nothing }) . parseJID =<<) <$> DB.get db (DB.byNode cheoJid ["joined"])
	case parseCommand txt thread existingRoom nick componentJid of
		Just JoinInvited -> do
			invitedRoom <- (parseJID =<<) <$> DB.get db (DB.byNode cheoJid ["invited"])
			let toJoin = invitedRoom >>= \jid -> parseJID (bareTxt jid <> s"/" <> nick)
			case toJoin of
				Just room ->
					(++) <$>
					leaveRoom db cheoJid "Joined a different room." <*>
					joinRoom db cheoJid room
				Nothing -> return [mkStanzaRec $ mkSMS componentJid smsJid (s"You have not recently been invited to a group")]
		Just JoinInvitedWrong
			| Just room <- existingRoom -> sendToRoom cheoJid room (s"Join")
			| otherwise -> do
				invitedRoom <- (parseJID =<<) <$> DB.get db (DB.byNode cheoJid ["invited"])
				let toJoin = invitedRoom >>= \jid -> parseJID (bareTxt jid <> fromString "/" <> nick)
				case toJoin of
					Just room ->
						fmap ((mkStanzaRec $ mkSMS componentJid smsJid (s"I think you meant \"/join\", trying anyway...")):)
						(joinRoom db cheoJid room)
					Nothing -> return [mkStanzaRec $ mkSMS componentJid smsJid (s"You have not recently been invited to a group")]
		Just (Create name) -> do
			servers <- shuffleM conferenceServers
			roomCreateStanzas <- createRoom componentJid servers cheoJid name
			if null roomCreateStanzas then
				return [mkStanzaRec $ mkSMS componentJid smsJid (s"Invalid group name")]
			else
				return roomCreateStanzas
		Just (Join room) -> do
			leaveRoom db cheoJid "Joined a different room."
			bookmarks <- DB.smembers db (DB.byNode cheoJid ["bookmarks"])
			let tel = maybe mempty strNode (jidNode cheoJid)
			joinRoom db cheoJid $
				fromMaybe room $ parseJID =<< fmap (<> fromString "/" <> nick)
				(find (mucShortMatch tel (strDomain $ jidDomain room)) bookmarks)
		Just Leave -> leaveRoom db cheoJid "Typed /leave"
		Just Who -> do
			let room = maybe mempty bareTxt existingRoom
			presence <- DB.hgetall db (DB.Key ["presence", T.unpack room])
			let presence' = filter (/= nick) $ map fst presence
			if null presence then
				return [mkStanzaRec $ mkSMS componentJid smsJid $ fromString $
						"You are not joined to a group. Reply with /help to learn more"
					]
			else
				return [mkStanzaRec $ mkSMS componentJid smsJid $ mconcat $ [
					s"You are joined to ", room,
					s" as ", nick] ++ if null presence' then [] else [
					s" along with\n",
					intercalate (s", ") presence'
				]]
		Just List -> do
			bookmarks <- DB.smembers db (DB.byNode cheoJid ["bookmarks"])
			return [mkStanzaRec $ mkSMS componentJid smsJid $ s"Groups you can /join\n" ++ intercalate (s"\n") bookmarks]
		Just (InviteCmd jid)
			| Just room <- existingRoom ->
				sendInvite db jid (Invite room cheoJid Nothing Nothing)
			| otherwise -> return [mkStanzaRec $ mkSMS componentJid smsJid (fromString "You are not joined to a group. Reply with /help to learn more")]
		Just (SetNick nick) -> do
			DB.set db (DB.byNode cheoJid ["nick"]) nick
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
			| otherwise -> do
				recentlyTold <- fromMaybe False <$> DB.getEnum db (DB.byNode smsJid ["recently-told-not-joined"])
				if (not recentlyTold) then do
					DB.setEnum db (DB.byNode smsJid ["recently-told-not-joined"]) True
					DB.expire db (DB.byNode smsJid ["recently-told-not-joined"]) (60*60)
					return [mkStanzaRec $ mkSMS componentJid smsJid (fromString "You are not joined to a group")]
				else do
					log "RECENTLY TOLD" smsJid
					return []
		Just (Debounce time) -> do
			DB.set db (DB.byNode cheoJid ["debounce"]) (tshow time)
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
			DB.set db (DB.byJid addjid ["addtoken"]) (tshow (formatJID cheoJid, token))
			return $ case parseJID (formatJID componentJid ++ s"/token") of
				Just sendFrom -> [mkStanzaRec $ mkSMS sendFrom smsJid (s"CHEOGRAM" ++ token)]
				Nothing -> []
		Just (DelJid deljid) -> do
			-- Deleting a JID is much less dangerous since in the worst case SMS just go to the actual phone number
			DB.del db (DB.byJid deljid ["cheoJid"])
			DB.srem db (DB.byNode cheoJid ["owners"]) [bareTxt deljid]

			return [mkStanzaRec $ mkSMS componentJid smsJid (bareTxt deljid ++ s" removed from your phone number")]
		Just Jids -> do
			owners <- DB.smembers db (DB.byNode cheoJid ["owners"])
			return [mkStanzaRec $ mkSMS componentJid smsJid $ s"JIDs owning this phone number:\n" <> intercalate (s"\n") owners]
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
	JoinError   JID |
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
	go state (JoinError mucJid) =
		-- Delete state, so next ping will happen and fail and attempt rejoin
		next $! Map.delete mucJid state
	go state (ForceRejoin mucJid cheoJid) = do
		atomically $ writeTChan toRoomPresences (StartRejoin cheoJid mucJid)
		mapM_ sendToComponent =<< rejoinRoom db cheoJid mucJid True
		next $! Map.insert mucJid Rejoining state
	go state CheckPings = do
		(next =<<) $! DB.foldKeysM db (DB.Key ["presence"]) state $ \state pkey@(DB.Key keyparts) -> do
			let Just muc = parseJID . fromString =<< atZ keyparts 1
			presences <- mapMaybe (ourJids muc) <$> DB.hgetall db pkey
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
	GetRoomPresences JID JID (TMVar [(Text, Maybe Text)])

roomPresences db toRoomPresences =
	forever $ atomically (readTChan toRoomPresences) >>= go
	where
	go (RecordSelfJoin cheoJid from jid) = do
		-- After a join is done we have a full presence list, remove old ones
		DB.del db (DB.byNode cheoJid [muc from, "old_presence"])
		globalAndLocal cheoJid from (\k -> DB.hset db k [(resource from, bareTxt <$> jid)])
	go (RecordJoin cheoJid from jid) =
		globalAndLocal cheoJid from (\k -> DB.hset db k [(resource from, bareTxt <$> jid)])
	go (RecordPart cheoJid from) = do
		globalAndLocal cheoJid from (\k -> DB.hdel db k [resource from])
	go (RecordNickChanged cheoJid from nick) =
		globalAndLocal cheoJid from (\k -> DB.hset db k [(resource from, Just nick)])
	go (Clear cheoJid from) =
		DB.del db (DB.byNode cheoJid [muc from, "presence"])
	go (StartRejoin cheoJid from) = do
		-- Copy current presences to a holding space so we can clear when rejoin is over
		presences <- DB.hgetall db (DB.byNode cheoJid [muc from, "presence"])
		DB.hset db (DB.byNode cheoJid [muc from, "old_presence"]) presences
		DB.del db (DB.byNode cheoJid [muc from, "presence"])
	go (GetRoomPresences cheoJid from rtrn) = do
		presences <- DB.hgetall db (DB.byNode cheoJid [muc from, "presence"])
		old_presences <- DB.hgetall db (DB.byNode cheoJid [muc from, "old_presence"])
		atomically $ putTMVar rtrn $ presences ++ old_presences

	globalAndLocal cheoJid from f = do
		f (DB.Key ["presence", muc from])
		f (DB.byNode cheoJid [muc from, "presence"])
	muc = T.unpack . bareTxt
	resource x = fromMaybe mempty (strResource <$> jidResource x)

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
		when (isNothing $ lookup nick presences) $ do
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
				expire <- fmap (fromMaybe (-1) . (readZ . textToString =<<)) (DB.get db (DB.byNode cheoJid ["debounce"]))
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


adhocBotManager :: (UIO.Unexceptional m) => DB.DB -> ([StatsD.Stat] -> UIO ()) -> JID -> (XMPP.Message -> UIO.UIO ()) -> (XMPP.IQ -> UIO.UIO (STM (Maybe XMPP.IQ))) -> (STM XMPP.Message) -> m ()
adhocBotManager db pushStatsd componentJid sendMessage sendIQ messages = do
	cleanupChan <- atomicUIO newTChan
	statefulManager cleanupChan Map.empty
	where
	statefulManager cleanupChan sessions = do
		join $ atomicUIO $ (processMessage cleanupChan sessions <$> messages) <|> (cleanupSession cleanupChan sessions <$> readTChan cleanupChan)

	cleanupSession cleanupChan sessions sessionToClean = statefulManager cleanupChan $! (Map.delete sessionToClean sessions)

	processMessage cleanupChan sessions message = do
		-- XXX: At some point this should not include resource, but it makes it easy to test for now
		UIO.lift $ pushStatsd [StatsD.stat ["adhoc-bot", "msg-recv"] 1 "c" Nothing]

		let key = bareTxt <$> (XMPP.stanzaFrom message)
		sessions' <- case Map.lookup key sessions of
			Just input -> input message >> return sessions
			Nothing -> do
				newChan <- atomicUIO newTChan

				UIO.forkFinally (adhocBotSession db componentJid sendMessage sendIQ (readTChan newChan) message) (\result -> do
						pushStatsd [StatsD.stat ["adhoc-bot", "cmd-run"] 1 "c" Nothing]
						fromIO_ $ either (log "adhocBotManager") (const $ return ()) result
						atomicUIO $ writeTChan cleanupChan key
					)
				let writer = (atomicUIO . writeTChan newChan)
				return $ Map.insert key writer sessions
		statefulManager cleanupChan sessions'

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

	magic <- magicOpen [MagicMimeType]
	magicLoadDefault magic

	args <- getArgs
	case args of
		("register":componentHost:host:port:secret:backendHost:did:password:[]) -> do
			log "" "Registering..."
			let Just componentJid = parseJID (fromString componentHost)
			let Just gatewayJid = parseJID (fromString backendHost)
			void $ runComponent (Server componentJid host (read port)) (fromString secret) $ do
				mapM_ putStanza =<< registerToGateway componentJid gatewayJid (fromString did) (fromString password)
				liftIO $ threadDelay 1000000
		[config] -> do
			(Config.Config componentJid (Config.ServerConfig host port) secret backendHost rawdid registrationJid conferences s5bListenOn (Config.ServerConfig s5bhost s5bport) jingleStore jingleStoreURL (Config.Redis presenceRCI stateRCI) (Config.ServerConfig statsdHost statsdPort) maybeAvatarPath) <- Dhall.input Dhall.auto (fromString config)
			log "" "Starting..."
			let Just did = normalizeTel rawdid
			db <- DB.mk stateRCI
			presenceRedis <- Redis.checkedConnect presenceRCI
			toJoinPartDebouncer <- atomically newTChan
			sendToComponent <- atomically newTChan
			toStanzaProcessor <- atomically newTChan
			toRoomPresences <- atomically newTChan
			toRejoinManager <- atomically newTChan

			statsd <- openStatsD statsdHost (show statsdPort) ["cheogram"]
			let pushStatsd = void . UIO.fromIO . StatsD.push statsd

			(sendIQ, iqReceiver) <- iqManager $ atomicUIO . writeTChan sendToComponent . mkStanzaRec
			adhocBotMessages <- atomically newTChan
			void $ forkFinally (adhocBotManager db pushStatsd componentJid (atomicUIO . writeTChan sendToComponent . mkStanzaRec) sendIQ (readTChan adhocBotMessages)) (log "adhocBotManagerTOP")

			void $ forkFinally (void $ joinPartDebouncer db backendHost (atomically . writeTChan sendToComponent) componentJid toRoomPresences toJoinPartDebouncer) (log "joinPartDebouncerTOP")
			void $ forkFinally (void $ roomPresences db toRoomPresences) (log "roomPresencesTOP")

			void $ forkIO $ forever $ atomically (writeTChan toRejoinManager CheckPings) >> threadDelay 120000000
			void $ forkFinally (void $ rejoinManager db (atomically . writeTChan sendToComponent) (textToString $ formatJID componentJid) toRoomPresences toRejoinManager) (log "rejoinManagerTOP")

			processDirectMessageRouteConfig <- ConfigureDirectMessageRoute.main (XMPP.jidDomain componentJid)
				(\userJid ->
					let userJid' = maybeUnescape componentJid userJid in
					(parseJID =<<) <$> DB.get db (DB.byJid userJid' ["possible-route"])
				)
				(\userJid -> do
					let userJid' = maybeUnescape componentJid userJid
					res <- (JidSwitch.fromAssoc) <$> DB.hgetall db (DB.byJid userJid' ["jidSwitch"])
					return $ fmap (\(x,y) -> (userJid', x, y)) res
				)
				(\userJid ->
					let userJid' = maybeUnescape componentJid userJid in
					(parseJID =<<) <$> DB.get db (DB.byJid userJid' ["direct-message-route"])
				)
				(\userJid mgatewayJid -> do
					let userJid' = maybeUnescape componentJid userJid
					DB.del db (DB.byJid userJid' ["possible-route"])
					case mgatewayJid of
						Just gatewayJid -> do
							maybeExistingRoute <- (parseJID =<<) <$> DB.get db (DB.byJid userJid' ["direct-message-route"])
							forM_ maybeExistingRoute $ \existingRoute ->
								when (existingRoute /= gatewayJid)
									(atomically . writeTChan sendToComponent . mkStanzaRec =<< unregisterDirectMessageRoute db componentJid userJid' existingRoute)

							DB.set db (DB.byJid userJid' ["direct-message-route"]) (formatJID gatewayJid)

							forM_ (parseJID $ escapeJid (bareTxt userJid') ++ s"@" ++ formatJID componentJid) $ \from ->
								forM_ (parseJID $ did ++ s"@" ++ formatJID gatewayJid) $ \to ->
									atomically $ writeTChan sendToComponent $ mkStanzaRec $
										mkSMS from to (s"/addjid " ++ bareTxt userJid')

							return ()
						Nothing -> do
							maybeExistingRoute <- (parseJID =<<) <$> DB.get db (DB.byJid userJid' ["direct-message-route"])
							deleteDirectMessageRoute db userJid'
							forM_ maybeExistingRoute $ \existingRoute ->
								atomically . writeTChan sendToComponent . mkStanzaRec =<< unregisterDirectMessageRoute db componentJid userJid' existingRoute
				)
				(\userJid ->
					let userJid' = maybeUnescape componentJid userJid in
					DB.del db (DB.byJid userJid' ["jidSwitch"])
				)
				(\userJid ->
					let userJid' = maybeUnescape componentJid userJid in
					DB.getEnum db (DB.byJid userJid' ["allowJidDiscovery"])
				)
				(\userJid allow ->
					let userJid' = maybeUnescape componentJid userJid in
					DB.setEnum db (DB.byJid userJid' ["allowJidDiscovery"]) allow
				)

			jingleHandler <- UIO.runEitherIO $ Jingle.setupJingleHandlers jingleStore s5bListenOn (fromString s5bhost, s5bport)
				(log "JINGLE")
				(\iq@(IQ { iqPayload = Just jingle }) path ->
					forM_ (isNamed (s"{urn:xmpp:jingle:1}content") =<< elementChildren jingle) $ \content -> do
					let fileDesc = mfilter (/=mempty) $ fmap (mconcat . elementText) $ headZ (isNamed (s"{urn:xmpp:jingle:apps:file-transfer:5}desc") =<< elementChildren =<< isNamed (s"{urn:xmpp:jingle:apps:file-transfer:5}file") =<< elementChildren =<< isNamed (s"{urn:xmpp:jingle:apps:file-transfer:5}description") =<< elementChildren content)
					mimeType <- fromIO_ $ magicFile magic path
					let extSuffix = maybe mempty (s"." ++) $ SMap.lookup mimeType mimeToExtMap
					atomicUIO $ writeTChan toStanzaProcessor $
						let url = jingleStoreURL ++ (T.takeWhileEnd (/='/') $ fromString path) ++ extSuffix in
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


			maybeAvatar <- mapM mkAvatar maybeAvatarPath

			log "" "runComponent STARTING"

			UIO.lift $ pushStatsd [StatsD.stat ["service", "start"] 1 "c" Nothing]

			log "runComponent ENDED" =<< runComponent (Server componentJid host port) secret
				(component db presenceRedis (UIO.lift . pushStatsd) backendHost did maybeAvatar (cacheOOB magic (UIO.lift . pushStatsd) jingleStore jingleStoreURL) sendIQ iqReceiver (writeTChan adhocBotMessages) toRoomPresences toRejoinManager toJoinPartDebouncer sendToComponent toStanzaProcessor processDirectMessageRouteConfig jingleHandler componentJid [registrationJid] conferences)
		_ -> log "ERROR" "Bad arguments"
