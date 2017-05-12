{-# LANGUAGE PackageImports #-}
import Prelude (show, read)
import BasicPrelude hiding (show, read, forM, mapM, forM_, mapM_, getArgs, log)
import System.IO (stdout, stderr, hSetBuffering, BufferMode(LineBuffering))
import Data.Char
import Control.Concurrent
import Control.Concurrent.STM
import Data.Foldable (forM_, mapM_, toList)
import Data.Traversable (forM, mapM)
import System.Environment (getArgs)
import Control.Error (readZ, syncIO, runEitherT, readMay, MaybeT(..), hoistMaybe)
import Data.Time (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Network (PortID(PortNumber))
import System.Random (Random(randomR), getStdRandom)
import System.Random.Shuffle (shuffleM)
import Data.Digest.Pure.SHA (sha1, bytestringDigest)

import "monads-tf" Control.Monad.Error (catchError) -- ick
import Data.XML.Types (Element(..), Node(NodeContent, NodeElement), Name(Name), Content(ContentText), isNamed, hasAttributeText, elementText, elementChildren, attributeText)
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Map as Map
import qualified Data.UUID as UUID ( toString )
import qualified Data.UUID.V1 as UUID ( nextUUID )
import qualified Data.ByteString.Lazy as LZ
import qualified Data.ByteString.Base64 as Base64
import qualified Database.TokyoCabinet as TC
import Network.Protocol.XMPP -- should import qualified

import Util
import qualified ConfigureDirectMessageRoute

instance Ord JID where
	compare x y = compare (show x) (show y)

data StanzaRec = StanzaRec (Maybe JID) (Maybe JID) (Maybe Text) (Maybe Text) [Element] Element deriving (Show)
mkStanzaRec x = StanzaRec (stanzaTo x) (stanzaFrom x) (stanzaID x) (stanzaLang x) (stanzaPayloads x) (stanzaToElement x)
instance Stanza StanzaRec where
	stanzaTo (StanzaRec to _ _ _ _ _) = to
	stanzaFrom (StanzaRec _ from _ _ _ _) = from
	stanzaID (StanzaRec _ _ id _ _ _) = id
	stanzaLang (StanzaRec _ _ _ lang _ _) = lang
	stanzaPayloads (StanzaRec _ _ _ _ payloads _) = payloads
	stanzaToElement (StanzaRec _ _ _ _ _ element) = element

mkSMS from to txt = (emptyMessage MessageChat) {
	messageTo = Just to,
	messageFrom = Just from,
	messagePayloads = [Element (fromString "{jabber:component:accept}body") [] [NodeContent $ ContentText txt]]
}

tcKey jid key = fmap (\node -> (T.unpack $ strNode node) <> "\0" <> key) (jidNode jid)
tcGetJID db jid key = liftIO $ case tcKey jid key of
	Just tck -> (parseJID . fromString =<<) <$> TC.runTCM (TC.get db tck)
	Nothing -> return Nothing
tcPutJID db cheoJid key jid = tcPut db cheoJid key $ T.unpack $ formatJID jid
tcPut db cheoJid key val = liftIO $ do
	let Just tck = tcKey cheoJid key
	True <- TC.runTCM (TC.put db tck val)
	return ()

getBody ns = listToMaybe . fmap (mconcat . elementText) . (isNamed (Name (fromString "body") (Just $ fromString ns) Nothing) <=< messagePayloads)

queryDisco to from = do
	uuid <- (fmap.fmap) (fromString . UUID.toString) UUID.nextUUID
	return [mkStanzaRec $ (emptyIQ IQGet) {
		iqTo = Just to,
		iqFrom = Just from,
		iqID = uuid,
		iqPayload = Just $ Element (fromString "{http://jabber.org/protocol/disco#info}query") [] []
	}]

queryCommandList to from = do
	uuid <- (fmap.fmap) (fromString . UUID.toString) UUID.nextUUID
	return [mkStanzaRec $ (emptyIQ IQGet) {
		iqTo = Just to,
		iqFrom = Just from,
		iqID = uuid,
		iqPayload = Just $ Element (fromString "{http://jabber.org/protocol/disco#items}query") [
			(s"{http://jabber.org/protocol/disco#items}node", [ContentText $ s"http://jabber.org/protocol/commands"])
		] []
	}]

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

nickFor db jid existingRoom
	| fmap bareTxt existingRoom == Just bareFrom = return $ fromMaybe (fromString "nonick") resourceFrom
	| Just tel <- normalizeTel =<< strNode <$> jidNode jid = do
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

cheogramAvailable from to =
	(emptyPresence PresenceAvailable) {
		presenceTo = Just to,
		presenceFrom = Just from,
		presencePayloads = [
			Element (s"{http://jabber.org/protocol/caps}c") [
				(s"{http://jabber.org/protocol/caps}hash", [ContentText $ fromString "sha-1"]),
				(s"{http://jabber.org/protocol/caps}node", [ContentText $ fromString "xmpp:cheogram.com"]),
				-- gateway/sms//Cheogram<jabber:iq:gateway<jabber:iq:register<urn:xmpp:ping<vcard-temp<
				(s"{http://jabber.org/protocol/caps}ver", [ContentText $ fromString "XCOs6r/FNTOQJwgYkKOjkktq8XI="])
			] []
		]
	}

telDiscoFeatures = [
		s"http://jabber.org/protocol/muc",
		s"jabber:x:conference",
		s"urn:xmpp:ping",
		s"urn:xmpp:receipts",
		s"vcard-temp"
	]

telCapsStr extraVars =
	s"client/sms//Cheogram<" ++ mconcat (intersperse (s"<") (sort (nub (telDiscoFeatures ++ extraVars)))) ++ s"<"

telAvailable from to disco =
	(emptyPresence PresenceAvailable) {
		presenceTo = Just to,
		presenceFrom = Just from,
		presencePayloads = [
			Element (s"{http://jabber.org/protocol/caps}c") [
				(s"{http://jabber.org/protocol/caps}hash", [ContentText $ fromString "sha-1"]),
				(s"{http://jabber.org/protocol/caps}node", [ContentText $ fromString "xmpp:cheogram.com"]),
				(s"{http://jabber.org/protocol/caps}ver", [ContentText hash])
			] []
		]
	}
	where
	hash = T.decodeUtf8 $ Base64.encode $ LZ.toStrict $ bytestringDigest $ sha1 $ LZ.fromStrict $ T.encodeUtf8 $ telCapsStr disco

telDiscoInfo id from to disco =
	(emptyIQ IQResult) {
		iqTo = Just to,
		iqFrom = Just from,
		iqID = Just id,
		iqPayload = Just $ Element (fromString "{http://jabber.org/protocol/disco#info}query") [] $
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

commandList componentJid id from to extras =
	(emptyIQ IQResult) {
		iqTo = Just to,
		iqFrom = Just from,
		iqID = id,
		iqPayload = Just $ Element (s"{http://jabber.org/protocol/disco#items}query")
			[(s"{http://jabber.org/protocol/disco#items}node", [ContentText $ s"http://jabber.org/protocol/commands"])]
			([
				NodeElement $ Element (s"{http://jabber.org/protocol/disco#items}item") [
						(s"{http://jabber.org/protocol/disco#items}jid", [ContentText $ formatJID componentJid ++ s"/" ++ ConfigureDirectMessageRoute.nodeName]),
						(s"{http://jabber.org/protocol/disco#items}node", [ContentText $ ConfigureDirectMessageRoute.nodeName]),
						(s"{http://jabber.org/protocol/disco#items}name", [ContentText $ s"Configure direct message route"])
				] []
			] ++ extraItems)
	}
	where
	extraItems = map (\el ->
			NodeElement $ el {
				elementAttributes = map (\(aname, acontent) ->
					if aname == s"{http://jabber.org/protocol/disco#items}jid" || aname == s"jid" then
						(aname, [ContentText $ formatJID componentJid ++ s"/route-command"])
					else
						(aname, acontent)
				) (elementAttributes el)
			}
		) extras

routeQueryOrReply db componentJid from smsJid resource query reply = do
	maybeRoute <- TC.runTCM $ TC.get db (T.unpack (bareTxt from) ++ "\0direct-message-route")
	case (fmap fromString maybeRoute, maybeRouteFrom) of
		(Just route, Just routeFrom) ->
				let routeTo = fromMaybe componentJid $ parseJID $ (maybe mempty (++ s"@") $ strNode <$> jidNode smsJid) ++ route in
				query routeTo routeFrom
		_ -> return [mkStanzaRec $ reply]
	where
	maybeRouteFrom = parseJID $ escapeJid (bareTxt from) ++ s"@" ++ formatJID componentJid ++ s"/" ++ (fromString resource)

routeDiscoOrReply db componentJid from smsJid resource reply =
	routeQueryOrReply db componentJid from smsJid resource queryDisco reply

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
			log "TO DIRECT ROUTE" route
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
		log "GOT INVITE" (invite, m)
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
	log "MESSAGE FROM GROUP" (existingRoom, body)
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
	log "WHISPER" (from, smsJid, body)

	ack <- case isNamed (fromString "{urn:xmpp:receipts}request") =<< messagePayloads m of
		(_:_) ->
			routeDiscoOrReply db componentJid from smsJid ("CHEOGRAM%query-then-send-ack%" ++ extra)
				(deliveryReceipt (fromMaybe mempty $ messageID m) to from)
		[] -> return []

	fmap (++ack) $ toRouteOrFallback db componentJid bareFrom resourceFrom smsJid m $ do
		nick <- nickFor db from existingRoom
		let txt = mconcat [fromString "(", nick, fromString " whispers) ", body]
		return [mkStanzaRec $ mkSMS componentJid smsJid txt]
	where
	extra = T.unpack $ escapeJid $ T.pack $ show (fromMaybe mempty (messageID m), fromMaybe mempty resourceFrom)
componentMessage _ _ m _ _ _ _ _ = do
	log "UNKNOWN MESSAGE" m
	return []

handleJoinPartRoom db toRoomPresences toRejoinManager toJoinPartDebouncer componentJid existingRoom from to smsJid payloads join
	| join,
	  [x] <- isNamed (fromString "{http://jabber.org/protocol/muc#user}x") =<< payloads,
	  not $ null $ code "110" =<< isNamed (fromString "{http://jabber.org/protocol/muc#user}status") =<< elementChildren x = do
		log "JOINED" (to, from)
		existingInvite <- tcGetJID db to "invited"
		when (existingInvite == parseJID bareMUC) $ do
			let Just invitedKey = tcKey to "invited"
			True <- TC.runTCM $ TC.out db invitedKey
			log "JOINED" (to, from, "INVITE CLEARED")
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
				log "JOINED" (to, from, "CREATED")
				uuid <- fromMaybe "UUIDFAIL" <$> (fmap.fmap) (fromString . UUID.toString) UUID.nextUUID
				let fullid = if (T.unpack resourceFrom `elem` map fst presences) then uuid else "CHEOGRAMCREATE%" <> uuid
				return [mkStanzaRec $ (emptyIQ IQGet) {
					iqTo = Just room,
					iqFrom = Just to,
					iqID = Just $ fromString fullid,
					iqPayload = Just $ Element (fromString "{http://jabber.org/protocol/muc#owner}query") [] []
				}]
			(_:_) | isNothing (lookup (T.unpack resourceFrom) presences) -> do
				log "JOINED" (to, from, resourceFrom, presences, "YOU HAVE JOINED")
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
		log "CHANGED NICK" (to, x)
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
		log "YOU HAVE LEFT" (to, existingRoom)
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
	| join = do
		log "UNKNOWN JOIN" (existingRoom, from, to, payloads, join)
		atomically $ writeTChan toRoomPresences $ RecordJoin to from (participantJid payloads)
		return []
	| otherwise = do
		log "UNKNOWN NOT JOIN" (existingRoom, from, to, payloads, join)
		atomically $ writeTChan toRoomPresences $ RecordPart to from
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
	log "REGISTERVERIFIFCATION" (to, iq)
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
	log "HANDLEVERIFICATIONCODE" (password, iq, time, codeAndTime)
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
	log "HANDLEREGISTER IQGet" (time, codeAndTime, iq)
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
		log "HANDLEREGISTER IQSet jabber:x:data phone" iq
		registerVerification db componentJid to iq
handleRegister db componentJid iq@(IQ { iqType = IQSet }) query
	| [phoneEl] <- isNamed (fromString "{jabber:iq:register}phone") =<< elementChildren query,
	  Just to <- (`telToJid` formatJID componentJid) $ T.filter isDigit $ mconcat (elementText phoneEl) = do
		log "HANDLEREGISTER IQSet jabber:iq:register phone" iq
		registerVerification db componentJid to iq
handleRegister db componentJid iq@(IQ { iqType = IQSet }) query
	| [form] <- isNamed (fromString "{jabber:x:data}x") =<< elementChildren query,
	  Just password <- getFormField form (fromString "password") = do
		log "HANDLEREGISTER IQSet jabber:x:data password" iq
		handleVerificationCode db componentJid password iq
handleRegister db componentJid iq@(IQ { iqType = IQSet, iqPayload = Just payload }) query
	| [passwordEl] <- isNamed (fromString "{jabber:iq:register}password") =<< elementChildren query = do
		log "HANDLEREGISTER IQSet jabber:iq:register password" iq
		handleVerificationCode db componentJid (mconcat $ elementText passwordEl) iq
handleRegister db componentJid iq@(IQ { iqType = IQSet }) query
	| [_] <- isNamed (fromString "{jabber:iq:register}remove") =<< elementChildren query = do
		log "HANDLEREGISTER IQSet jabber:iq:register remove" iq
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

componentStanza db _ _ _ _ _ _ componentJid (ReceivedMessage (m@Message { messageTo = Just (JID { jidNode = Nothing }), messageFrom = Just from}))
	| Just _ <- getBody "jabber:component:accept" m = return [
			mkStanzaRec $ mkSMS componentJid from (s"Instead of sending messages to " ++ formatJID componentJid ++ s" directly, you can SMS your contacts by sending messages to +1<phone-number>@" ++ formatJID componentJid ++ s" Jabber IDs.  Or, for support, come talk to us in xmpp:discuss@conference.soprani.ca?join")
		]
	| otherwise = log "WEIRD BODYLESS MESSAGE DIRECT TO COMPONENT" m >> return []
componentStanza _ _ _ _ _ _ _ _ (ReceivedMessage (m@Message { messageTo = Just to, messageFrom = Just from}))
	| [x] <- isNamed (fromString "{http://jabber.org/protocol/muc#user}x") =<< messagePayloads m,
	  not $ null $ code "104" =<< isNamed (fromString "{http://jabber.org/protocol/muc#user}status") =<< elementChildren x = do
		log "CODE104" (to, from)
		queryDisco from to
componentStanza db (Just smsJid) _ _ _ _ _ componentJid (ReceivedMessage (m@Message { messageTo = Just to, messageFrom = Just from})) = do
	log "RECEIVEDMESSAGE" m
	existingRoom <- tcGetJID db to "joined"
	componentMessage db componentJid m existingRoom (bareTxt from) resourceFrom smsJid $
		getBody "jabber:component:accept" m
	where
	resourceFrom = strResource <$> jidResource from
componentStanza _ (Just smsJid) _ _ toRejoinManager _ _ componentJid (ReceivedPresence p@(Presence { presenceType = PresenceError, presenceFrom = Just from, presenceTo = Just to, presenceID = Just id }))
	| fromString "CHEOGRAMREJOIN%" `T.isPrefixOf` id = do
		log "FAILED TO REJOIN, try again in 10s" p
		void $ forkIO $ threadDelay 10000000 >> atomically (writeTChan toRejoinManager $ ForceRejoin from to)
		return []
	| otherwise = do
		log "FAILED TO JOIN" p
		let errorText = maybe mempty (mconcat . (fromString "\n":) . elementText) $ listToMaybe $
			isNamed (fromString "{urn:ietf:params:xml:ns:xmpp-stanzas}text") =<<
			elementChildren =<< isNamed (fromString "{jabber:component:accept}error") =<< presencePayloads p
		return [mkStanzaRec $ mkSMS componentJid smsJid (fromString "* Failed to join " <> bareTxt from <> errorText)]
componentStanza db (Just smsJid) _ toRoomPresences toRejoinManager toJoinPartDebouncer _ componentJid (ReceivedPresence (Presence {
		presenceType = typ,
		presenceFrom = Just from,
		presenceTo = Just to,
		presencePayloads = payloads
	})) | typ `elem` [PresenceAvailable, PresenceUnavailable] = do
		existingRoom <- tcGetJID db to "joined"
		log "JOIN PART ROOM" (from, to, typ, existingRoom, payloads)
		handleJoinPartRoom db toRoomPresences toRejoinManager toJoinPartDebouncer componentJid existingRoom from to smsJid payloads (typ == PresenceAvailable)
componentStanza _ _ _ _ _ _ _ _ (ReceivedPresence (Presence { presenceType = PresenceSubscribe, presenceFrom = Just from, presenceTo = Just to@JID { jidNode = Nothing } })) = do
	log "SUBSCRIBE GATEWAY" (from, to)
	return [
			mkStanzaRec $ (emptyPresence PresenceSubscribed) {
				presenceTo = Just from,
				presenceFrom = Just to
			},
			mkStanzaRec $ (emptyPresence PresenceSubscribe) {
				presenceTo = Just from,
				presenceFrom = Just to
			},
			mkStanzaRec $ cheogramAvailable to from
		]
componentStanza db (Just smsJid) _ _ _ _ _ componentJid (ReceivedPresence (Presence { presenceType = PresenceSubscribe, presenceFrom = Just from, presenceTo = Just to@JID { jidNode = Just _ } })) = do
	log "SUBSCRIBE TEL" (from, to)
	stanzas <- routeDiscoOrReply db componentJid from smsJid "CHEOGRAM%query-then-send-presence" $ telAvailable to from []
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
componentStanza _ _ _ _ _ _ _ _ (ReceivedPresence (Presence { presenceType = PresenceProbe, presenceFrom = Just from, presenceTo = Just to@JID { jidNode = Nothing } })) = do
	log "RESPOND TO PROBES" (from, to)
	return [mkStanzaRec $ cheogramAvailable to from]
componentStanza db (Just smsJid) _ _ _ _ _ componentJid (ReceivedPresence (Presence { presenceType = PresenceProbe, presenceFrom = Just from, presenceTo = Just to@JID { jidNode = Just _ } })) = do
	log "RESPOND TO TEL PROBES" smsJid
	routeDiscoOrReply db componentJid from smsJid "CHEOGRAM%query-then-send-presence" $ telAvailable to from []
componentStanza _ _ registrationJids _ _ _ processDirectMessageRouteConfig componentJid (ReceivedIQ (IQ { iqType = IQSet, iqTo = Just to, iqFrom = Just from, iqID = Just id, iqPayload = Just p }))
	| jidNode to == Nothing,
	  [iqEl] <- isNamed (s"{jabber:client}iq") =<< elementChildren =<< isNamed (s"{urn:xmpp:forward:0}forwarded") p,
	  [payload] <- isNamed (s"{http://jabber.org/protocol/commands}command") =<< elementChildren iqEl,
	  Just asFrom <- parseJID =<< attributeText (s"from") iqEl,
	  bareTxt from `elem` map bareTxt registrationJids = do
		log "COMMAND ON BEHALF OF" (from, asFrom, payload)
		replyIQ <- processDirectMessageRouteConfig $ (emptyIQ IQSet) {
				iqID = Just id,
				iqTo = Just to,
				iqFrom = Just asFrom,
				iqPayload = Just payload
			}
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
componentStanza _ _ _ _ _ _ processDirectMessageRouteConfig componentJid (ReceivedIQ iq@(IQ { iqTo = Just to, iqPayload = payload }))
	| fmap strResource (jidResource to) == Just (s"CHEOGRAM%" ++ ConfigureDirectMessageRoute.nodeName),
	  Just (fwdBy, onBehalf, iqId) <- readZ . T.unpack =<< iqID iq = do
		log "FWD BY" (fwdBy, onBehalf, iqId, iq)
		replyIQ <- processDirectMessageRouteConfig (iq { iqID = iqId })
		let fromLocalpart = maybe mempty (\localpart -> localpart++s"@") (fmap strNode . jidNode =<< iqFrom replyIQ)
		return [mkStanzaRec $ replyIQ {
			iqTo = if fmap bareTxt (iqTo replyIQ) == Just onBehalf then parseJID fwdBy else iqTo replyIQ,
			iqFrom = parseJID (fromLocalpart ++ formatJID componentJid ++ s"/CHEOGRAM%" ++ ConfigureDirectMessageRoute.nodeName)
		}]
componentStanza _ _ _ _ _ _ processDirectMessageRouteConfig componentJid (ReceivedIQ iq@(IQ { iqTo = Just to, iqPayload = payload }))
	| (jidNode to == Nothing && fmap elementName payload == Just (s"{http://jabber.org/protocol/commands}command") && (attributeText (s"node") =<< payload) == Just ConfigureDirectMessageRoute.nodeName) ||
	  fmap strResource (jidResource to) == Just (s"CHEOGRAM%" ++ ConfigureDirectMessageRoute.nodeName) = do
		log "PART OF COMMAND" iq
		replyIQ <- processDirectMessageRouteConfig iq
		let fromLocalpart = maybe mempty (\localpart -> localpart++s"@") (fmap strNode . jidNode =<< iqFrom replyIQ)
		return [mkStanzaRec $ replyIQ {
			iqFrom = parseJID (fromLocalpart ++ formatJID componentJid ++ s"/CHEOGRAM%" ++ ConfigureDirectMessageRoute.nodeName)
		}]
componentStanza db _ _ _ _ _ _ componentJid (ReceivedIQ iq@(IQ { iqFrom = Just _, iqTo = Just (JID { jidNode = Nothing }), iqPayload = Just p }))
	| iqType iq `elem` [IQGet, IQSet],
	  [query] <- isNamed (fromString "{jabber:iq:register}query") p = do
		log "LOOKS LIKE REGISTRATION" iq
		return [mkStanzaRec $ iqNotImplemented iq]
componentStanza db _ _ _ _ _ _ componentJid (ReceivedIQ (IQ { iqType = IQGet, iqFrom = Just from, iqTo = Just to, iqID = id, iqPayload = Just p }))
	| Nothing <- jidNode to,
	  [_] <- isNamed (fromString "{http://jabber.org/protocol/disco#info}query") p = do
		log "DISCO ON US" (from, to, p)
		return [mkStanzaRec $ (emptyIQ IQResult) {
			iqTo = Just from,
			iqFrom = Just to,
			iqID = id,
			iqPayload = Just $ Element (fromString "{http://jabber.org/protocol/disco#info}query") []
				[
					NodeElement $ Element (fromString "{http://jabber.org/protocol/disco#info}identity") [
						(fromString "{http://jabber.org/protocol/disco#info}category", [ContentText $ fromString "gateway"]),
						(fromString "{http://jabber.org/protocol/disco#info}type", [ContentText $ fromString "sms"]),
						(fromString "{http://jabber.org/protocol/disco#info}name", [ContentText $ fromString "Cheogram"])
					] [],
					NodeElement $ Element (fromString "{http://jabber.org/protocol/disco#info}feature") [
						(fromString "{http://jabber.org/protocol/disco#info}var", [ContentText $ fromString "jabber:iq:gateway"])
					] [],
					NodeElement $ Element (fromString "{http://jabber.org/protocol/disco#info}feature") [
						(fromString "{http://jabber.org/protocol/disco#info}var", [ContentText $ fromString "jabber:iq:register"])
					] [],
					NodeElement $ Element (fromString "{http://jabber.org/protocol/disco#info}feature") [
						(fromString "{http://jabber.org/protocol/disco#info}var", [ContentText $ fromString "urn:xmpp:ping"])
					] [],
					NodeElement $ Element (fromString "{http://jabber.org/protocol/disco#info}feature") [
						(fromString "{http://jabber.org/protocol/disco#info}var", [ContentText $ fromString "vcard-temp"])
					] []
				]
		}]
	| Nothing <- jidNode to,
	  [s"http://jabber.org/protocol/commands"] ==
	    mapMaybe (attributeText (s"node")) (isNamed (fromString "{http://jabber.org/protocol/disco#items}query") p) = do
		log "componentStanza QUERY FOR COMMAND LIST" to
		routeQueryOrReply db componentJid from componentJid ("CHEOGRAM%query-then-send-command-list%" ++ extra) queryCommandList (commandList componentJid id to from [])
	| Nothing <- jidNode to,
	  [_] <- isNamed (s"{vcard-temp}vCard") p =
		return [mkStanzaRec $ (emptyIQ IQResult) {
			iqTo = Just from,
			iqFrom = Just to,
			iqID = id,
			iqPayload = Just $ Element (s"{vcard-temp}vCard") []
				[
					NodeElement $ Element (s"{vcard-temp}URL") [] [NodeContent $ ContentText $ s"https://cheogram.com"],
					NodeElement $ Element (s"{vcard-temp}DESC") [] [NodeContent $ ContentText $ s"Cheogram provides stable JIDs for PSTN identifiers, with routing through many possible backends.\n\n© Stephen Paul Weber, licensed under AGPLv3+.\n\nSource code for this gateway is available from the listed homepage.\n\nPart of the Soprani.ca project."]
				]
		}]
	where
	extra = T.unpack $ escapeJid $ T.pack $ show (id, fromMaybe mempty resourceFrom)
	resourceFrom = strResource <$> jidResource from
componentStanza db (Just smsJid) _ _ _ _ _ componentJid (ReceivedIQ (IQ { iqType = IQGet, iqFrom = Just from, iqTo = Just to, iqID = Just id, iqPayload = Just p }))
	| Just _ <- jidNode to,
	  [_] <- isNamed (fromString "{http://jabber.org/protocol/disco#info}query") p = do
		log "DISCO ON USER" (from, to, p)
		routeDiscoOrReply db componentJid from smsJid ("CHEOGRAM%query-then-send-disco-info%" ++ extra) $
			telDiscoInfo id to from []
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
componentStanza _ _ _ _ _ _ _ componentJid (ReceivedIQ (iq@IQ { iqType = IQSet, iqFrom = Just from, iqTo = Just (to@JID {jidNode = Nothing}), iqID = id, iqPayload = Just p }))
	| [query] <- isNamed (fromString "{jabber:iq:gateway}query") p,
	  [prompt] <- isNamed (fromString "{jabber:iq:gateway}prompt") =<< elementChildren query = do
		log "jabber:iq:gateway submit" (from, to, p)
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
componentStanza _ _ _ _ _ _ _ _ (ReceivedIQ (IQ { iqType = IQGet, iqFrom = Just from, iqTo = Just (to@JID {jidNode = Nothing}), iqID = id, iqPayload = Just p }))
	| [_] <- isNamed (fromString "{jabber:iq:gateway}query") p = do
		log "jabber:iq:gateway query" (from, to, p)
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
componentStanza db _ _ _ _ _ _ componentJid (ReceivedIQ (iq@IQ { iqType = IQError, iqFrom = Just from, iqTo = Just to }))
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
componentStanza _ _ _ _ _ _ _ componentJid (ReceivedIQ (iq@IQ { iqType = IQResult, iqFrom = Just from, iqTo = Just to }))
	| (strNode <$> jidNode to) == Just (fromString "create"),
	  Just resource <- strResource <$> jidResource to = do
		log "create@ RESULT" (from, to, iq)
		case T.splitOn (fromString "|") resource of
			(cheoJidT:name:[]) | Just cheoJid <- parseJID cheoJidT, Just tel <- strNode <$> jidNode cheoJid ->
				createRoom componentJid [strDomain $ jidDomain from] cheoJid (name <> fromString "_" <> tel)
			(cheoJidT:name:servers) | Just cheoJid <- parseJID cheoJidT ->
				createRoom componentJid servers cheoJid name
			_ -> return [] -- Invalid packet, ignore
componentStanza _ _ _ _ toRejoinManager _ _ _ (ReceivedIQ (iq@IQ { iqType = IQResult, iqID = Just id, iqFrom = Just from }))
	| fromString "CHEOGRAMPING%" `T.isPrefixOf` id = do
		log "PING RESULT" from
		atomically $ writeTChan toRejoinManager (PingReply from)
		return []
componentStanza _ _ _ _ toRejoinManager _ _ _ (ReceivedIQ (iq@IQ { iqType = IQError, iqID = Just id, iqFrom = Just from }))
	| fromString "CHEOGRAMPING%" `T.isPrefixOf` id = do
		log "PING ERROR RESULT" from
		atomically $ writeTChan toRejoinManager (PingError from)
		return []
componentStanza _ (Just smsJid) _ _ _ _ _ componentJid (ReceivedIQ (iq@IQ { iqType = IQError, iqFrom = Just from, iqTo = Just to })) = do
	log "IQ ERROR" iq
	return [mkStanzaRec $ mkSMS componentJid smsJid (fromString "Error while querying or configuring " <> formatJID from)]
componentStanza _ _ _ _ _ _ _ _ (ReceivedIQ (IQ { iqType = IQResult, iqFrom = Just from, iqTo = Just to, iqID = Just id, iqPayload = Just p }))
	| [query] <- isNamed (fromString "{http://jabber.org/protocol/muc#owner}query") p,
	  [form] <- isNamed (fromString "{jabber:x:data}x") =<< elementChildren query = do
		log "MUC DISCO RESULT" (from, to, p)
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
componentStanza _ (Just smsJid) _ _ _ _ _ componentJid (ReceivedIQ (iq@IQ { iqType = IQResult, iqFrom = Just from, iqTo = Just to, iqID = Just id }))
	| fromString "CHEOGRAMCREATE%" `T.isPrefixOf` id = do
		log "CHEOGRAMCREATE RESULT YOU HAVE CREATED" (from, to, iq)
		fmap (((mkStanzaRec $ mkSMS componentJid smsJid (mconcat [fromString "* You have created ", bareTxt from])):) . concat . toList) $
			forM (parseJID $ bareTxt to <> fromString "/create") $
				queryDisco from
componentStanza db _ _ _ _ _ _ componentJid (ReceivedIQ iq@(IQ { iqType = typ, iqTo = Just to@(JID { jidNode = Just toNode }), iqPayload = Just p }))
	| typ `elem` [IQResult, IQError],
	  Just idAndResource <- T.stripPrefix (s"CHEOGRAM%query-then-send-command-list%") . strResource =<< jidResource to,
	  Just (iqId, resource) <- readZ $ T.unpack $ unescapeJid idAndResource,
	  Just routeTo <- parseJID (unescapeJid (strNode toNode) ++ if T.null resource then mempty else s"/" ++ resource) =
		if typ == IQError then do
			log "ERROR FROM ROUTE, SEND DEFAULT COMMAND LIST" iq
			return [mkStanzaRec $ commandList componentJid iqId componentJid routeTo []]
		else do
			log "COMMANDS FROM ROUTE, MERGE WITH OURS AND SEND" iq
			let items = isNamed (s"{http://jabber.org/protocol/disco#items}item") =<< elementChildren p
			return [mkStanzaRec $ commandList componentJid iqId componentJid routeTo items]
componentStanza db _ _ _ _ _ _ componentJid (ReceivedIQ (IQ { iqType = IQResult, iqTo = Just to@(JID { jidNode = Just toNode }), iqFrom = Just from, iqPayload = Just p }))
	| Just idAndResource <- T.stripPrefix (s"CHEOGRAM%query-then-send-ack%") . strResource =<< jidResource to,
	  Just (messageId, resource) <- readZ $ T.unpack $ unescapeJid idAndResource,
	  [query] <- isNamed (fromString "{http://jabber.org/protocol/disco#info}query") p,
	  Just routeTo <- parseJID (unescapeJid (strNode toNode) ++ if T.null resource then mempty else s"/" ++ resource),
	  Just fromNode <- jidNode from,
	  Just routeFrom <- parseJID (strNode fromNode ++ s"@" ++ formatJID componentJid) =
		let features = mapMaybe (attributeText (fromString "var")) $ isNamed (fromString "{http://jabber.org/protocol/disco#info}feature") =<< elementChildren query in
		if (s"urn:xmpp:receipts") `elem` features then do
			log "DISCO RESULT, DO NOT SEND ACK" (from, to, features)
			return []
		else do
			log "DISCO RESULT, NOW SEND ACK" (from, to, routeFrom, routeTo, features)
			return [mkStanzaRec $ deliveryReceipt messageId routeFrom routeTo]
	| Just idAndResource <- T.stripPrefix (s"CHEOGRAM%query-then-send-disco-info%") . strResource =<< jidResource to,
	  Just (iqID, resource) <- readZ $ T.unpack $ unescapeJid idAndResource,
	  [query] <- isNamed (fromString "{http://jabber.org/protocol/disco#info}query") p,
	  Just routeTo <- parseJID (unescapeJid (strNode toNode) ++ if T.null resource then mempty else s"/" ++ resource),
	  Just fromNode <- jidNode from,
	  Just routeFrom <- parseJID (strNode fromNode ++ s"@" ++ formatJID componentJid) = do
		log "DISCO RESULT, NOW SEND INFO ONWARD" (from, to, routeFrom, routeTo)
		return [
				mkStanzaRec $ telDiscoInfo iqID routeFrom routeTo $ mapMaybe (attributeText (fromString "var")) $
				isNamed (fromString "{http://jabber.org/protocol/disco#info}feature") =<< elementChildren query
			]
	| fmap strResource (jidResource to) == Just (s"CHEOGRAM%query-then-send-presence"),
	  [query] <- isNamed (fromString "{http://jabber.org/protocol/disco#info}query") p,
	  Just routeTo <- parseJID (unescapeJid (strNode toNode)),
	  Just fromNode <- jidNode from,
	  Just routeFrom <- parseJID (strNode fromNode ++ s"@" ++ formatJID componentJid) = do
		log "DISCO RESULT, NOW SEND PRESENCE" (from, to, routeFrom, routeTo)
		return [
				mkStanzaRec $ telAvailable routeFrom routeTo $ mapMaybe (attributeText (fromString "var")) $
				isNamed (fromString "{http://jabber.org/protocol/disco#info}feature") =<< elementChildren query
			]
	| [query] <- isNamed (fromString "{http://jabber.org/protocol/disco#info}query") p = do
		log "DISCO RESULT" (from, to, p)
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
componentStanza _ _ _ _ _ _ _ _ (ReceivedIQ (iq@IQ { iqType = IQGet, iqFrom = Just from, iqTo = Just to, iqPayload = Just p }))
	| not $ null $ isNamed (fromString "{urn:xmpp:ping}ping") p = do
		log "urn:xmpp:ping" (from, to)
		return [mkStanzaRec $ iq {
			iqTo = Just from,
			iqFrom = Just to,
			iqType = IQResult,
			iqPayload = Nothing
		}]
componentStanza db maybeSmsJid _ _ _ _ _ componentJid (ReceivedIQ (iq@IQ { iqType = typ, iqFrom = Just from })) = do
	let resourceSuffix = maybe mempty (s"/"++) $ fmap strResource (jidResource from)
	maybeRoute <- TC.runTCM $ TC.get db (T.unpack (bareTxt from) ++ "\0direct-message-route")
	case (fmap fromString maybeRoute, parseJID $ escapeJid (bareTxt from) ++ s"@" ++ formatJID componentJid ++ resourceSuffix) of
		(Just route, Just routeFrom) -> do
			log "IQ ROUTE" route
			return [mkStanzaRec $ iq {
				iqFrom = Just routeFrom,
				iqTo = parseJID $ (maybe mempty (++s"@") $ strNode <$> (jidNode =<< maybeSmsJid)) ++ route
			}]
		_ | typ `elem` [IQGet, IQSet] -> do
			log "REPLY WITH IQ ERROR (no route)" iq
			return [mkStanzaRec $ iqNotImplemented iq]
		_ -> log "IGNORE BOGUS REPLY (no route)" iq >> return []
componentStanza _ _ _ _ _ _ _ _ s = do
	log "UNKNOWN STANZA" s
	return []

participantJid payloads =
	listToMaybe $ mapMaybe (parseJID <=< attributeText (fromString "jid")) $
	isNamed (fromString "{http://jabber.org/protocol/muc#user}item") =<<
	elementChildren =<<
	isNamed (fromString "{http://jabber.org/protocol/muc#user}x") =<< payloads

component db backendHost toRoomPresences toRejoinManager toJoinPartDebouncer toComponent processDirectMessageRouteConfig componentJid registrationJids conferenceServers = do
	thread <- forkXMPP $ forever $ flip catchError (log "component EXCEPTION") $ do
		stanza <- liftIO $ atomically $ readTChan toComponent
		log "COMPONENT OUT" stanza

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

	flip catchError (\e -> liftIO (log "component part 2 EXCEPTION" e >> killThread thread)) $ forever $ do
		stanza <- getStanza
		log "COMPONENT  IN" stanza
		liftIO $ case (stanzaFrom $ receivedStanza stanza, stanzaTo $ receivedStanza stanza, mapToBackend backendHost =<< stanzaTo (receivedStanza stanza), fmap strNode . jidNode =<< stanzaTo (receivedStanza stanza), stanza) of
			(Just from, Just to, _, _, _)
				| strDomain (jidDomain from) == backendHost,
				  to == componentJid ->
					case stanza of
						(ReceivedMessage m@(Message { messageType = MessageError })) ->
							log "backend error" stanza
						(ReceivedMessage m)
							| Just txt <- getBody "jabber:component:accept" m,
							  Just cheoJid <- mapToComponent from ->
								mapM_ sendToComponent =<< processSMS db componentJid conferenceServers from cheoJid txt
						_ -> log "backend no match" stanza
			(Just from, Just to, Nothing, Just localpart, ReceivedMessage m)
				| Just txt <- getBody "jabber:component:accept" m,
				  (T.length txt == 144 || T.length txt == 145) && (s"CHEOGRAM") `T.isPrefixOf` txt -> do -- the length of our token messages
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
				| fmap (((s"CHEOGRAM%") `T.isPrefixOf`) . strResource) (jidResource to) /= Just True -> do
					let toResourceSuffix = maybe mempty (s"/"++) (strResource <$> jidResource to)
					maybeRoute <- TC.runTCM $ TC.get db (T.unpack (unescapeJid localpart) ++ "\0direct-message-route")
					case (fmap fromString maybeRoute, parseJID (unescapeJid localpart ++ toResourceSuffix), mapToComponent from) of
						(Just route, Just routeTo, Just componentFrom) | route == strDomain (jidDomain from) -> do
							log "FROM DIRECT ROUTE" stanza
							sendToComponent $ receivedStanzaFromTo componentFrom routeTo stanza
						_ | Just jid <- (`telToJid` formatJID componentJid) =<< strNode <$> jidNode to -> do
							log "MESSAGE INVALID JID" stanza
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
						  | otherwise -> do
							log "MESSAGE UNKNOWN JID" stanza
							sendToComponent $ stanzaError stanza $
								Element (fromString "{jabber:component:accept}error")
								[(fromString "{jabber:component:accept}type", [ContentText $ fromString "cancel"])]
								[NodeElement $ Element (fromString "{urn:ietf:params:xml:ns:xmpp-stanzas}item-not-found") [] []]
			(_, _, backendTo, _, _) ->
				mapM_ sendToComponent =<< componentStanza db backendTo registrationJids toRoomPresences toRejoinManager toJoinPartDebouncer processDirectMessageRouteConfig componentJid stanza
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

	receivedStanzaFromTo from to (ReceivedMessage m) = mkStanzaRec $ m {
			messageFrom = Just from,
			messageTo = Just to
		}
	receivedStanzaFromTo from to (ReceivedPresence p) = mkStanzaRec $ p {
			presenceFrom = Just from,
			presenceTo = Just to
		}
	receivedStanzaFromTo from to (ReceivedIQ iq) = mkStanzaRec $ iq {
			iqFrom = Just from,
			iqTo = Just to
		}

	receivedStanza (ReceivedMessage m) = mkStanzaRec m
	receivedStanza (ReceivedPresence p) = mkStanzaRec p
	receivedStanza (ReceivedIQ iq) = mkStanzaRec iq

mapToBackend backendHost (JID { jidNode = Just node })
	| Just ('+', tel) <- T.uncons localpart,
	  T.all isDigit tel = result
	| Just _ <- parsePhoneContext localpart = result
	| otherwise = Nothing
	where
	result = parseJID (localpart ++ s"@" ++ backendHost)
	localpart = strNode node
mapToBackend backendHost (JID { jidNode = Nothing }) = parseJID backendHost

normalizeTel fullTel
	| Just ('+',e164) <- T.uncons fullTel,
	  T.all isDigit e164 = Just fullTel
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
			parseJIDrequireNode jid <|>
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
			parseJIDrequireNode to <|>
			telToJid to (formatJID componentJid) <|>
			(parseJID =<< fmap (\r -> bareTxt r <> fromString "/" <> to) room)
		) <*> pure msg
	| Just stime <- stripCIPrefix (fromString "/debounce ") txt,
	  Just time <- readMay (textToString stime) = Just $ Debounce time
	| citxt == fromString "/join" = Just JoinInvited
	| citxt == fromString "join" = Just JoinInvitedWrong
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

sendToRoom cheoJid room msg = do
	log "SEND TO ROOM" (cheoJid, room, msg)
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
	log "LEAVE ROOM" (existingRoom, cheoJid, reason)
	return $ (flip map) (toList existingRoom) $ \leaveRoom ->
		mkStanzaRec $ (emptyPresence PresenceUnavailable) {
			presenceTo = Just leaveRoom,
			presenceFrom = Just cheoJid,
			presencePayloads = [Element (fromString "{jabber:component:accept}status") [] [NodeContent $ ContentText $ fromString reason]]
		}

joinRoom db cheoJid room =
	rejoinRoom db cheoJid room False

rejoinRoom db cheoJid room rejoin = do
	log "JOIN ROOM" (room, cheoJid)
	password <- maybe (return Nothing) (TC.runTCM . TC.get db) (tcKey cheoJid (T.unpack (bareTxt room) <> "\0muc_roomsecret"))
	let pwEl = maybe [] (\pw -> [
			NodeElement $ Element (fromString "{http://jabber.org/protocol/muc}password") [] [NodeContent $ ContentText $ fromString pw]
		]) password

	uuid <- fromMaybe "UUIDFAIL" <$> (fmap.fmap) (fromString . UUID.toString) UUID.nextUUID
	return [mkStanzaRec $ (emptyPresence PresenceAvailable) {
		presenceID = Just $ fromString $ (if rejoin then "CHEOGRAMREJOIN%" else "") <> uuid,
		presenceTo = Just room,
		presenceFrom = Just cheoJid,
		presencePayloads = [Element (fromString "{http://jabber.org/protocol/muc}x") [] ([
			NodeElement $ Element (fromString "{http://jabber.org/protocol/muc}history") [(fromString "{http://jabber.org/protocol/muc}maxchars", [ContentText $ fromString "0"])] []
		] <> pwEl)]
	}]

addMUCOwner room from jid = do
	log "ADD MUC OWNER" (room, from, jid)
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
	log "SEND INVITE" (room, to, from)
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
			log "go state CheckPings" $ fromString "Checking (ping?) participants in " <> formatJID muc <> fromString "..."
			presences <- fmap (mapMaybe (ourJids muc) . fromMaybe [] . (readZ =<<)) (TC.runTCM $ TC.get db pkey)
			(\x -> foldM x state presences) $ \state (mucJid, cheoJid) ->
				case Map.lookup mucJid state of
					Nothing -> do
						log "PINGING" (mucJid, cheoJid)
						uuid <- fromMaybe "UUIDFAIL" <$> (fmap.fmap) (fromString . UUID.toString) UUID.nextUUID
						sendToComponent $ mkStanzaRec $ (emptyIQ IQGet) {
							iqTo = Just mucJid,
							iqFrom = Just cheoJid,
							iqID = Just $ fromString $ "CHEOGRAMPING%" <> uuid,
							iqPayload = Just $ Element (fromString "{urn:xmpp:ping}ping") [] []
						}
						return $! Map.insert mucJid (PingSent cheoJid) state
					Just (PingSent _) -> do -- Timeout, rejoin
						log "PING TIMEOUT" (mucJid, cheoJid)
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
		log "STARTREJOIN" (cheoJid, muc from, presences, old_presences)
		tcPut db cheoJid (muc from <> "\0old_presence")
			(show (presences <> old_presences :: [(String, Maybe String)]))
		forM_ (tcKey cheoJid (muc from <> "\0presence")) (TC.runTCM . TC.out db)
	go (GetRoomPresences cheoJid from rtrn) = do
		presences <- (fromMaybe [] . (readZ =<<)) <$>
			maybe (return Nothing) (TC.runTCM . TC.get db) (tcKey cheoJid (muc from <> "\0presence"))
		old_presences <- (fromMaybe [] . (readZ =<<)) <$>
			maybe (return Nothing) (TC.runTCM . TC.get db) (tcKey cheoJid (muc from <> "\0old_presence"))
		log "GETROOMPRESENCES" (cheoJid, from, presences, old_presences)
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
		log "DEBOUNCE JOIN/PART" (msg, state)
		go state msg >>= next

	recordJoinPart cheoJid from mjid join
		| join = atomically $ writeTChan toRoomPresences $ RecordJoin cheoJid from mjid
		| otherwise = atomically $ writeTChan toRoomPresences $ RecordPart cheoJid from

	sendPart cheoJid from time = forM_ (mapToBackend backendHost cheoJid) $ \smsJid -> do
		log "DEBOUNCE PART, GONNA SEND" (smsJid, from, time)
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
		log "DEBOUNCE JOIN, MAYBE GONNA SEND" (cheoJid, from, presences)
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

openTokyoCabinet :: (TC.TCDB a) => String -> IO a
openTokyoCabinet pth = TC.runTCM $ do
	db <- TC.new
	True <- TC.open db pth [TC.OREADER, TC.OWRITER, TC.OCREAT]
	return db

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
			void $ runComponent (Server componentJid host (PortNumber $ fromIntegral (read port :: Int))) (fromString secret) $ do
				mapM_ putStanza =<< registerToGateway componentJid gatewayJid (fromString did) (fromString password)
				liftIO $ threadDelay 1000000
		(name:host:port:secret:backendHost:rawdid:registration:conferences) -> do
			log "" "Starting..."
			let Just componentJid = parseJID (fromString name)
			let Just registrationJid = parseJID (fromString registration)
			let Just did = normalizeTel (fromString rawdid)
			db <- openTokyoCabinet "./db.tcdb" :: IO TC.HDB
			toJoinPartDebouncer <- atomically newTChan
			sendToComponent <- atomically newTChan
			toRoomPresences <- atomically newTChan
			toRejoinManager <- atomically newTChan

			void $ forkIO $ joinPartDebouncer db (fromString backendHost) (atomically . writeTChan sendToComponent) componentJid toRoomPresences toJoinPartDebouncer
			void $ forkIO $ roomPresences db toRoomPresences

			void $ forkIO $ forever $ atomically (writeTChan toRejoinManager CheckPings) >> threadDelay 120000000
			void $ forkIO $ rejoinManager db (atomically . writeTChan sendToComponent) name toRoomPresences toRejoinManager

			processDirectMessageRouteConfig <- ConfigureDirectMessageRoute.main
				(\userJid ->
					(parseJID . fromString =<<) <$> TC.runTCM (TC.get db (T.unpack (bareTxt userJid) ++ "\0direct-message-route"))
				)
				(\userJid mgatewayJid -> do
					log "SETTING DIRECT MESSAGE ROUTE" (userJid, mgatewayJid)
					case mgatewayJid of
						Just gatewayJid -> do
							maybeExistingRoute <- (parseJID . fromString =<<) <$> TC.runTCM (TC.get db (T.unpack (bareTxt userJid) ++ "\0direct-message-route"))
							forM_ maybeExistingRoute $ \existingRoute ->
								when (existingRoute /= gatewayJid)
									(atomically . writeTChan sendToComponent . mkStanzaRec =<< unregisterDirectMessageRoute db componentJid userJid existingRoute)

							True <- TC.runTCM $ TC.put db (T.unpack (bareTxt userJid) ++ "\0direct-message-route") (T.unpack $ formatJID gatewayJid)

							forM_ (parseJID $ escapeJid (bareTxt userJid) ++ s"@" ++ formatJID componentJid) $ \from ->
								forM_ (parseJID $ did ++ s"@" ++ formatJID gatewayJid) $ \to ->
									atomically $ writeTChan sendToComponent $ mkStanzaRec $
										mkSMS from to (s"/addjid " ++ bareTxt userJid)

							return ()
						Nothing -> do
							maybeExistingRoute <- (parseJID . fromString =<<) <$> TC.runTCM (TC.get db (T.unpack (bareTxt userJid) ++ "\0direct-message-route"))
							TC.runTCM $ TC.out db (T.unpack (bareTxt userJid) ++ "\0direct-message-route")
							forM_ maybeExistingRoute $ \existingRoute ->
								atomically . writeTChan sendToComponent . mkStanzaRec =<< unregisterDirectMessageRoute db componentJid userJid existingRoute
				)

			forever $ do
				log "" "runComponent STARTING"

				(log "runComponent ENDED" <=< (runEitherT . syncIO)) $
					runComponent (Server componentJid host (PortNumber $ fromIntegral (read port :: Int))) (fromString secret)
						(component db (fromString backendHost) toRoomPresences toRejoinManager toJoinPartDebouncer sendToComponent processDirectMessageRouteConfig componentJid [registrationJid] (map fromString conferences))
		_ -> log "ERROR" "Bad arguments"
