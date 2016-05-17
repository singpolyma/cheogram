{-# LANGUAGE PackageImports #-}
import Prelude (show, read)
import BasicPrelude hiding (show, read, forM_, mapM_, getArgs, log)
import System.IO (stdout, stderr, hSetBuffering, BufferMode(LineBuffering))
import Data.Char
import Control.Concurrent
import Control.Concurrent.STM
import Data.Foldable (forM_, mapM_, toList)
import System.Environment (getArgs)
import Control.Error (readZ, syncIO, runEitherT)
import Data.Time (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
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

instance Ord JID where
	compare x y = compare (show x) (show y)

log :: (Show a, MonadIO m) => String -> a -> m ()
log tag x = liftIO $ do
	time <- getCurrentTime
	putStr (fromString $ show time <> " " <> tag <> " :: ") >> print x >> putStrLn mempty

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

getFormField form var =
		listToMaybe $ mapMaybe (\node ->
			case node of
				NodeElement el
					| elementName el == fromString "{jabber:x:data}field" &&
					  (attributeText (fromString "{jabber:x:data}var") el == Just var ||
					  attributeText (fromString "var") el == Just var) ->
						Just $ mconcat $
						elementText =<< isNamed (fromString "{jabber:x:data}value") =<< elementChildren el
				_ -> Nothing
		) (elementNodes form)

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

componentMessage _ toVitelity _ (m@Message { messageType = MessageError }) _ _ _ tel body = do
	log "MESSAGE ERROR"  m
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
componentMessage db toVitelity toComponent m@(Message { messageTo = Just to }) existingRoom _ _ tel _
	| Just invite <- getMediatedInvitation m <|> getDirectInvitation m = do
		log "GOT INVITE" (invite, m)
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
			regJid <- tcGetJID db tel "registered"
			forM_ regJid $ \jid -> sendInvite db toComponent jid (invite { inviteFrom = to })
componentMessage _ toVitelity _ (m@Message { messageType = MessageGroupChat }) existingRoom bareFrom resourceFrom tel (Just body) = do
	log "MESSAGE FROM GROUP" (existingRoom, body)
	if fmap bareTxt existingRoom == Just bareFrom && (
	   existingRoom /= parseJID (bareFrom <> fromString "/" <> fromMaybe mempty resourceFrom) ||
	   not (fromString "CHEOGRAM%" `T.isPrefixOf` fromMaybe mempty (messageID m))) then
		writeStanzaChan toVitelity $ mkSMS tel txt
	else
		log "MESSAGE FROM WRONG GROUP" (fmap bareTxt existingRoom, bareFrom, m)
	where
	txt = mconcat [fromString "(", fromMaybe (fromString "nonick") resourceFrom, fromString ") ", body]
componentMessage db toVitelity _ (Message { messageFrom = Just from }) existingRoom _ _ tel (Just body) = do
	log "WHISPER" (from, tel, body)
	nick <- nickFor db from existingRoom
	let txt = mconcat [fromString "(", nick, fromString " whispers) ", body]
	writeStanzaChan toVitelity $ mkSMS tel txt
componentMessage _ _ _ m _ _ _ _ _ = log "UNKNOWN MESSAGE" m

handleJoinPartRoom db toVitelity toRoomPresences toRejoinManager toJoinPartDebouncer toComponent existingRoom from to tel payloads join
	| join,
	  [x] <- isNamed (fromString "{http://jabber.org/protocol/muc#user}x") =<< payloads,
	  not $ null $ code "110" =<< isNamed (fromString "{http://jabber.org/protocol/muc#user}status") =<< elementChildren x = do
		log "JOINED" (tel, from)
		existingInvite <- tcGetJID db tel "invited"
		when (existingInvite == parseJID bareMUC) $ do
			True <- TC.runTCM $ TC.out db $ tcKey tel "invited"
			log "JOINED" (tel, from, "INVITE CLEARED")
			return ()
		tcPutJID db tel "joined" from
		bookmarks <- fmap (fromMaybe [] . (readZ =<<)) (TC.runTCM $ TC.get db (tcKey tel "bookmarks"))
		True <- TC.runTCM (TC.put db (tcKey tel "bookmarks") (show $ sort $ nub $ T.unpack bareMUC : bookmarks))

		presences <- syncCall toRoomPresences $ GetRoomPresences tel from
		atomically $ writeTChan toRoomPresences $ RecordJoin tel from (Just to)

		atomically $ writeTChan toRejoinManager $ Joined from

		case presences of
			[] -> do -- No one in the room, so we "created"
				log "JOINED" (tel, from, "CREATED")
				uuid <- fromMaybe "UUIDFAIL" <$> (fmap.fmap) (fromString . UUID.toString) UUID.nextUUID
				let fullid = if (T.unpack resourceFrom `elem` map fst presences) then uuid else "CHEOGRAMCREATE%" <> uuid
				writeStanzaChan toComponent $ (emptyIQ IQGet) {
					iqTo = Just room,
					iqFrom = Just to,
					iqID = Just $ fromString fullid,
					iqPayload = Just $ Element (fromString "{http://jabber.org/protocol/muc#owner}query") [] []
				}
			(_:_) | isNothing (lookup (T.unpack resourceFrom) presences) -> do
				log "JOINED" (tel, from, "YOU HAVE JOINED")
				writeStanzaChan toVitelity $ mkSMS tel $ mconcat [
						fromString "* You have joined ", bareMUC,
						fromString " as ", resourceFrom,
						fromString " along with\n",
						fromString $ intercalate ", " (filter (/= T.unpack resourceFrom) $ map fst presences)
					]
				queryDisco toComponent room to
			_ -> do
				log "JOINED" (tel, from, "FALSE PRESENCE")
				queryDisco toComponent room to
	| not join,
	  [x] <- isNamed (fromString "{http://jabber.org/protocol/muc#user}x") =<< payloads,
	  (_:_) <- code "303" =<< isNamed (fromString "{http://jabber.org/protocol/muc#user}status") =<< elementChildren x = do
		log "CHANGED NICK" (tel, x)
		mapM_ (\nick -> do
			atomically $ writeTChan toRoomPresences $ RecordNickChanged tel from nick
			writeStanzaChan toVitelity $ mkSMS tel $ mconcat [
					fromString "* ",
					resourceFrom,
					fromString " has changed their nick to ",
					nick
				]
			) $ attributeText (fromString "nick")
				=<< listToMaybe (isNamed (fromString "{http://jabber.org/protocol/muc#user}item") =<< elementChildren x)
	| not join,
	  [x] <- isNamed (fromString "{http://jabber.org/protocol/muc#user}x") =<< payloads,
	  (_:_) <- code "332" =<< isNamed (fromString "{http://jabber.org/protocol/muc#user}status") =<< elementChildren x = do
		log "SERVER RESTART, rejoin in 5s" (tel, from)
		void $ forkIO $ threadDelay 5000000 >> atomically (writeTChan toRejoinManager $ ForceRejoin from tel)
	| not join && existingRoom == Just from = do
		log "YOU HAVE LEFT" (tel, existingRoom)
		True <- TC.runTCM $ TC.out db $ tcKey tel "joined"
		atomically $ writeTChan toRoomPresences $ RecordPart tel from
		atomically $ writeTChan toRoomPresences $ Clear tel from
		writeStanzaChan toVitelity $ mkSMS tel (fromString "* You have left " <> bareMUC)
	| fmap bareTxt existingRoom == Just bareMUC && join = atomically $ writeTChan toJoinPartDebouncer $ DebounceJoin tel from (participantJid payloads)
	| fmap bareTxt existingRoom == Just bareMUC && not join = atomically $ writeTChan toJoinPartDebouncer $ DebouncePart tel from
	| join = do
		log "UNKNOWN JOIN" (existingRoom, from, to, tel, payloads, join)
		atomically $ writeTChan toRoomPresences $ RecordJoin tel from (participantJid payloads)
	| otherwise = do
		log "UNKNOWN NOT JOIN" (existingRoom, from, to, tel, payloads, join)
		atomically $ writeTChan toRoomPresences $ RecordPart tel from
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

data RegistrationCode = RegistrationCode { regCode :: Int,  tel :: Text, expires :: UTCTime } deriving (Show, Read)

sendRegisterVerification db toVitelity toComponent tel iq = do
	log "REGISTERVERIFIFCATION" (tel, iq)
	code <- getStdRandom (randomR (123457::Int,987653))
	time <- getCurrentTime
	True <- TC.runTCM $ TC.put db ((maybe mempty T.unpack $ bareTxt <$> iqFrom iq) <> "\0registration_code") $ show $ RegistrationCode code tel time
	writeStanzaChan toVitelity $ mkSMS tel $ fromString ("Enter this verification code to complete registration: " <> show code)
	writeStanzaChan toComponent $ iq {
			iqTo = iqFrom iq,
			iqFrom = iqTo iq,
			iqType = IQResult,
			iqPayload = Just verificationResponse
		}

handleVerificationCode db toComponent componentHost password iq = do
	time <- getCurrentTime
	codeAndTime <- fmap (readZ =<<) $ TC.runTCM $ TC.get db regKey
	log "HANDLEVERIFICATIONCODE" (password, iq, time, codeAndTime)
	if (fmap expires codeAndTime > Just ((-300) `addUTCTime` time)) then
		forM_ codeAndTime $ \RegistrationCode { regCode = code, tel = tel } ->
		case (show code == T.unpack password, iqTo iq, iqFrom iq) of
			(True, Just to, Just from) -> do
				writeStanzaChan toComponent $ iq {
					iqTo = iqFrom iq,
					iqFrom = iqTo iq,
					iqType = IQResult,
					iqPayload = Just $ Element (fromString "{jabber:iq:register}query") [] []
				}

				bookmarks <- fmap (fromMaybe [] . (readZ =<<)) (TC.runTCM $ TC.get db (tcKey tel "bookmarks"))
				forM_ (mapMaybe parseJID bookmarks) $ \bookmark ->
					sendInvite db toComponent from (Invite bookmark (fromMaybe to $ telToJid tel (formatJID to)) (Just $ fromString "Cheogram registration") Nothing)

				True <- TC.runTCM $ TC.put db (T.unpack (bareTxt from) <> "\0registered") (T.unpack tel)
				tcPutJID db tel "registered" from

				-- If there is a nick that doesn't end in _sms, add _sms
				nick <- TC.runTCM (TC.get db $ tcKey tel "nick")
				forM_ nick $ \nick -> do
					let nick' = (fromMaybe (fromString nick) $ T.stripSuffix (fromString "_sms") (fromString nick)) <> fromString "_sms"

					existingRoom <- (parseJID <=< fmap bareTxt) <$> tcGetJID db tel "joined"
					forM_ existingRoom $ \room -> do
						let toJoin = parseJID (bareTxt room <> fromString "/" <> nick')
						forM_ toJoin $ joinRoom db toComponent componentHost tel

					True <- TC.runTCM (TC.put db (tcKey tel "nick") (T.unpack nick'))
					return ()
			_ ->
				writeStanzaChan toComponent $ iq {
					iqTo = iqFrom iq,
					iqFrom = iqTo iq,
					iqType = IQError,
					iqPayload = Just $ Element (fromString "{jabber:component:accept}error")
						[(fromString "{jabber:component:accept}type", [ContentText $ fromString "auth"])]
						[NodeElement $ Element (fromString "{urn:ietf:params:xml:ns:xmpp-stanzas}not-authorized") [] []]
				}
	else
		void $ TC.runTCM $ TC.out db regKey
	where
	regKey = (maybe mempty T.unpack $ bareTxt <$> iqFrom iq) <> "\0registration_code"

handleRegister db _ toComponent _ iq@(IQ { iqType = IQGet }) _ = do
	time <- getCurrentTime
	codeAndTime <- fmap (readZ =<<) $ TC.runTCM $ TC.get db ((maybe mempty T.unpack $ bareTxt <$> iqFrom iq) <> "\0registration_code")
	log "HANDLEREGISTER IQGet" (time, codeAndTime, iq)
	if fmap expires codeAndTime > Just ((-300) `addUTCTime` time) then
		writeStanzaChan toComponent $ iq {
			iqTo = iqFrom iq,
			iqFrom = iqTo iq,
			iqType = IQResult,
			iqPayload = Just verificationResponse
		}
	else
		writeStanzaChan toComponent $ iq {
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
		}
handleRegister db toVitelity toComponent _ iq@(IQ { iqType = IQSet }) query
	| [form] <- isNamed (fromString "{jabber:x:data}x") =<< elementChildren query,
	  Just tel <- (normalizeTel . T.filter isDigit) =<< getFormField form (fromString "phone") = do
		log "HANDLEREGISTER IQSet jabber:x:data phone" iq
		sendRegisterVerification db toVitelity toComponent tel iq
handleRegister db toVitelity toComponent _ iq@(IQ { iqType = IQSet }) query
	| [phoneEl] <- isNamed (fromString "{jabber:iq:register}phone") =<< elementChildren query,
	  Just tel <- normalizeTel $ T.filter isDigit $ mconcat (elementText phoneEl) = do
		log "HANDLEREGISTER IQSet jabber:iq:register phone" iq
		sendRegisterVerification db toVitelity toComponent tel iq
handleRegister db toVitelity toComponent componentHost iq@(IQ { iqType = IQSet }) query
	| [form] <- isNamed (fromString "{jabber:x:data}x") =<< elementChildren query,
	  Just password <- getFormField form (fromString "password") = do
		log "HANDLEREGISTER IQSet jabber:x:data password" iq
		handleVerificationCode db toComponent componentHost password iq
handleRegister db toVitelity toComponent componentHost iq@(IQ { iqType = IQSet, iqPayload = Just payload }) query
	| [passwordEl] <- isNamed (fromString "{jabber:iq:register}password") =<< elementChildren query = do
		log "HANDLEREGISTER IQSet jabber:iq:register password" iq
		handleVerificationCode db toComponent componentHost (mconcat $ elementText passwordEl) iq
handleRegister db _ toComponent _ iq@(IQ { iqType = IQSet }) query
	| [_] <- isNamed (fromString "{jabber:iq:register}remove") =<< elementChildren query = do
		log "HANDLEREGISTER IQSet jabber:iq:register remove" iq
		tel <- maybe mempty T.pack <$> TC.runTCM (TC.get db $ T.unpack (maybe mempty bareTxt $ iqFrom iq) <> "\0registered")
		_ <- TC.runTCM $ TC.out db $ tcKey tel "registered"
		_ <- TC.runTCM $ TC.out db $ T.unpack (maybe mempty bareTxt $ iqFrom iq) <> "\0registered"
		writeStanzaChan toComponent $ iq {
			iqTo = iqFrom iq,
			iqFrom = iqTo iq,
			iqType = IQResult,
			iqPayload = Just $ Element (fromString "{jabber:iq:register}query") [] []
		}
handleRegister _ _ toComponent _ iq@(IQ { iqType = typ }) _
	| typ `elem` [IQGet, IQSet] = do
		log "HANDLEREGISTER return error" iq
		writeStanzaChan toComponent $ iq {
			iqTo = iqFrom iq,
			iqFrom = iqTo iq,
			iqType = IQError,
			iqPayload = Just $ Element (fromString "{jabber:component:accept}error")
				[(fromString "{jabber:component:accept}type", [ContentText $ fromString "cancel"])]
				[NodeElement $ Element (fromString "{urn:ietf:params:xml:ns:xmpp-stanzas}feature-not-implemented") [] []]
		}
handleRegister _ _ _ _ _ iq = log "HANDLEREGISTER UNKNOWN" iq

componentStanza _ _ _ _ _ toComponent _ (ReceivedMessage (m@Message { messageTo = Just to, messageFrom = Just from}))
	| [x] <- isNamed (fromString "{http://jabber.org/protocol/muc#user}x") =<< messagePayloads m,
	  not $ null $ code "104" =<< isNamed (fromString "{http://jabber.org/protocol/muc#user}status") =<< elementChildren x = do
		log "CODE104" (to, from)
		queryDisco toComponent from to
componentStanza db toVitelity _ _ _ toComponent componentHost (ReceivedMessage (m@Message { messageTo = Just to, messageFrom = Just from}))
	| Just tel <- strNode <$> jidNode to,
	  T.length tel == 11 && fromString "1" `T.isPrefixOf` tel = do
		log "RECEIVEDMESSAGE" m
		existingRoom <- tcGetJID db tel "joined"
		componentMessage db toVitelity toComponent m existingRoom (bareTxt from) resourceFrom tel $
			getBody "jabber:component:accept" m
	| Just jid <- (`telToJid` fromString componentHost) =<< strNode <$> jidNode to = do
		log "MESSAGE INVALID JID" m
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
	| otherwise = do
		log "MESSAGE UNKNOWN JID" m
		writeStanzaChan toComponent $ m {
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
componentStanza _ toVitelity _ toRejoinManager _ _ _ (ReceivedPresence p@(Presence { presenceType = PresenceError, presenceFrom = Just from, presenceTo = Just to, presenceID = Just id }))
	| fromString "CHEOGRAMREJOIN%" `T.isPrefixOf` id,
	  Just tel <- strNode <$> jidNode to = do
		log "FAILED TO REJOIN, try again in 10s" p
		void $ forkIO $ threadDelay 10000000 >> atomically (writeTChan toRejoinManager $ ForceRejoin from tel)
	| Just tel <- strNode <$> jidNode to = do
		log "FAILED TO JOIN" p
		let errorText = maybe mempty (mconcat . (fromString "\n":) . elementText) $ listToMaybe $
			isNamed (fromString "{urn:ietf:params:xml:ns:xmpp-stanzas}text") =<<
			elementChildren =<< isNamed (fromString "{jabber:component:accept}error") =<< presencePayloads p
		writeStanzaChan toVitelity $ mkSMS tel (fromString "* Failed to join " <> bareTxt from <> errorText)
componentStanza db toVitelity toRoomPresences toRejoinManager toJoinPartDebouncer toComponent _ (ReceivedPresence (Presence {
		presenceType = typ,
		presenceFrom = Just from,
		presenceTo = Just to@(JID { jidNode = Just toNode }),
		presencePayloads = payloads
	})) | typ `elem` [PresenceAvailable, PresenceUnavailable] = do
		existingRoom <- tcGetJID db (strNode toNode) "joined"
		log "JOIN PART ROOM" (from, to, typ, existingRoom, payloads)
		handleJoinPartRoom db toVitelity toRoomPresences toRejoinManager toJoinPartDebouncer toComponent existingRoom from to (strNode toNode) payloads (typ == PresenceAvailable)
componentStanza _ _ _ _ _ toComponent _ (ReceivedPresence (Presence { presenceType = PresenceSubscribe, presenceFrom = Just from, presenceTo = Just to@JID { jidNode = Nothing } })) = do
	log "APPROVE SUBSCRIPTION" (from, to)
	writeStanzaChan toComponent $ (emptyPresence PresenceSubscribed) {
		presenceTo = Just from,
		presenceFrom = Just to
	}
	log "SUBSCRIBE" (from, to)
	writeStanzaChan toComponent $ (emptyPresence PresenceSubscribe) {
		presenceTo = Just from,
		presenceFrom = Just to
	}
componentStanza _ _ _ _ _ toComponent _ (ReceivedPresence (Presence { presenceType = PresenceProbe, presenceFrom = Just from, presenceTo = Just to@JID { jidNode = Nothing } })) = do
	log "RESPOND TO PROBES" (from, to)
	writeStanzaChan toComponent $ (emptyPresence PresenceAvailable) {
		presenceTo = Just from,
		presenceFrom = Just to,
		presencePayloads = [
			Element (fromString "{http://jabber.org/protocol/caps}c") [
				(fromString "{http://jabber.org/protocol/caps}hash", [ContentText $ fromString "sha-1"]),
				(fromString "{http://jabber.org/protocol/caps}node", [ContentText $ fromString "xmpp:sms.cheogram.com"]),
				-- gateway/sms//Cheogram SMS Gateway<jabber:iq:gateway<jabber:iq:register<urn:xmpp:ping<
				(fromString "{http://jabber.org/protocol/caps}ver", [ContentText $ fromString "4/LEvjGRsHBQRu9D+1NwytYdFUY="])
			] []
		]
	}
componentStanza db toVitelity _ _ _ toComponent componentHost (ReceivedIQ iq@(IQ { iqFrom = Just _, iqTo = Just (JID { jidNode = Nothing }), iqPayload = Just p }))
	| iqType iq `elem` [IQGet, IQSet],
	  [query] <- isNamed (fromString "{jabber:iq:register}query") p = do
		log "LOOKS LIKE REGISTRATION" iq
		handleRegister db toVitelity toComponent componentHost iq query
componentStanza _ _ _ _ _ toComponent _ (ReceivedIQ (IQ { iqType = IQGet, iqFrom = Just from, iqTo = Just to, iqID = id, iqPayload = Just p }))
	| Nothing <- jidNode to,
	  [_] <- isNamed (fromString "{http://jabber.org/protocol/disco#info}query") p = do
		log "DISCO ON US" (from, to, p)
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
					] [],
					NodeElement $ Element (fromString "{http://jabber.org/protocol/disco#info}feature") [
						(fromString "{http://jabber.org/protocol/disco#info}var", [ContentText $ fromString "jabber:iq:gateway"])
					] [],
					NodeElement $ Element (fromString "{http://jabber.org/protocol/disco#info}feature") [
						(fromString "{http://jabber.org/protocol/disco#info}var", [ContentText $ fromString "jabber:iq:register"])
					] [],
					NodeElement $ Element (fromString "{http://jabber.org/protocol/disco#info}feature") [
						(fromString "{http://jabber.org/protocol/disco#info}var", [ContentText $ fromString "urn:xmpp:ping"])
					] []
				]
		}
componentStanza _ _ _ _ _ toComponent _ (ReceivedIQ (IQ { iqType = IQGet, iqFrom = Just from, iqTo = Just to, iqID = id, iqPayload = Just p }))
	| Just _ <- jidNode to,
	  [_] <- isNamed (fromString "{http://jabber.org/protocol/disco#info}query") p = do
		log "DISCO ON USER" (from, to, p)
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
					] [],
					NodeElement $ Element (fromString "{http://jabber.org/protocol/disco#info}feature") [
						(fromString "{http://jabber.org/protocol/disco#info}var", [ContentText $ fromString "urn:xmpp:ping"])
					] []
				]
		}
componentStanza _ _ _ _ _ toComponent componentHost (ReceivedIQ (iq@IQ { iqType = IQSet, iqFrom = Just from, iqTo = Just (to@JID {jidNode = Nothing}), iqID = id, iqPayload = Just p }))
	| [query] <- isNamed (fromString "{jabber:iq:gateway}query") p,
	  [prompt] <- isNamed (fromString "{jabber:iq:gateway}prompt") =<< elementChildren query = do
		log "jabber:iq:gateway submit" (from, to, p)
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
componentStanza _ _ _ _ _ toComponent _ (ReceivedIQ (IQ { iqType = IQGet, iqFrom = Just from, iqTo = Just (to@JID {jidNode = Nothing}), iqID = id, iqPayload = Just p }))
	| [_] <- isNamed (fromString "{jabber:iq:gateway}query") p = do
		log "jabber:iq:gateway query" (from, to, p)
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
componentStanza db _ _ _ _ toComponent componentHost (ReceivedIQ (iq@IQ { iqType = IQError, iqFrom = Just from, iqTo = Just to }))
	| (strNode <$> jidNode to) == Just (fromString "create"),
	  Just resource <- strResource <$> jidResource to = do
		log "create@ ERROR" (from, to, iq)
		case T.splitOn (fromString "|") resource of
			(tel:_) -> do
				nick <- maybe tel fromString <$> TC.runTCM (TC.get db $ tcKey tel "nick")
				let Just room = parseJID $ bareTxt from <> fromString "/" <> nick
				leaveRoom db toComponent componentHost tel "Joined a different room."
				joinRoom db toComponent componentHost tel room
			_ -> return () -- Invalid packet, ignore
componentStanza _ _ _ _ _ toComponent componentHost (ReceivedIQ (iq@IQ { iqType = IQResult, iqFrom = Just from, iqTo = Just to }))
	| (strNode <$> jidNode to) == Just (fromString "create"),
	  Just resource <- strResource <$> jidResource to = do
		log "create@ RESULT" (from, to, iq)
		case map T.unpack $ T.splitOn (fromString "|") resource of
			(tel:name:[]) -> void $ createRoom toComponent componentHost [T.unpack $ strDomain $ jidDomain from] tel (name <> "_" <> tel)
			(tel:name:servers) -> void $ createRoom toComponent componentHost servers tel name
			_ -> return () -- Invalid packet, ignore
componentStanza _ _ _ toRejoinManager _ _ _ (ReceivedIQ (iq@IQ { iqType = IQResult, iqID = Just id, iqFrom = Just from }))
	| fromString "CHEOGRAMPING%" `T.isPrefixOf` id = do
		log "PING RESULT" from
		atomically $ writeTChan toRejoinManager (PingReply from)
componentStanza _ _ _ toRejoinManager _ _ _ (ReceivedIQ (iq@IQ { iqType = IQError, iqID = Just id, iqFrom = Just from }))
	| fromString "CHEOGRAMPING%" `T.isPrefixOf` id = do
		log "PING ERROR RESULT" from
		atomically $ writeTChan toRejoinManager (PingError from)
componentStanza _ toVitelity _ _ _ _ _ (ReceivedIQ (iq@IQ { iqType = IQError, iqFrom = Just from, iqTo = Just to }))
	| Just tel <- strNode <$> jidNode to = do
		log "IQ ERROR" iq
		writeStanzaChan toVitelity $ mkSMS tel (fromString "Error while querying or configuring " <> formatJID from)
componentStanza _ _ _ _ _ toComponent _ (ReceivedIQ (IQ { iqType = IQResult, iqFrom = Just from, iqTo = Just to, iqID = Just id, iqPayload = Just p }))
	| [query] <- isNamed (fromString "{http://jabber.org/protocol/muc#owner}query") p,
	  [form] <- isNamed (fromString "{jabber:x:data}x") =<< elementChildren query = do
		log "DISCO RESULT" (from, to, p)
		uuid <- fromMaybe "UUIDFAIL" <$> (fmap.fmap) (fromString . UUID.toString) UUID.nextUUID
		let fullid = if fromString "CHEOGRAMCREATE%" `T.isPrefixOf` id then "CHEOGRAMCREATE%" <> uuid else uuid
		writeStanzaChan toComponent $ (emptyIQ IQSet) {
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
		}
componentStanza _ toVitelity _ _ _ toComponent _ (ReceivedIQ (iq@IQ { iqType = IQResult, iqFrom = Just from, iqTo = Just to, iqID = Just id }))
	| Just tel <- strNode <$> jidNode to,
	  fromString "CHEOGRAMCREATE%" `T.isPrefixOf` id = do
		log "CHEOGRAMCREATE RESULT YOU HAVE CREATED" (from, to, iq)
		writeStanzaChan toVitelity $ mkSMS tel (mconcat [fromString "* You have created ", bareTxt from])
		forM_ (parseJID $ bareTxt to <> fromString "/create") $
			queryDisco toComponent from
componentStanza db _ _ _ _ toComponent componentHost (ReceivedIQ (IQ { iqType = IQResult, iqTo = Just to, iqFrom = Just from, iqPayload = Just p }))
	| Just tel <- strNode <$> jidNode to,
	  [query] <- isNamed (fromString "{http://jabber.org/protocol/disco#info}query") p = do
		log "DISCO RESULT" (from, to, p)
		let vars = mapMaybe (attributeText (fromString "var")) $
			isNamed (fromString "{http://jabber.org/protocol/disco#info}feature") =<< elementChildren query
		let muc_membersonly = fromEnum $ fromString "muc_membersonly" `elem` vars
		True <- TC.runTCM $ TC.put db (T.unpack (formatJID from) <> "\0muc_membersonly") muc_membersonly
		when (fmap strResource (jidResource to) == Just (fromString "create")) $ do
			regJid <- tcGetJID db tel "registered"
			forM_ regJid $ \jid -> forM_ (parseJID $ bareTxt to) $ \to -> sendInvite db toComponent jid (Invite from to Nothing Nothing)
componentStanza _ _ _ _ _ toComponent _ (ReceivedIQ (iq@IQ { iqType = IQGet, iqFrom = Just from, iqTo = Just to, iqPayload = Just p }))
	| not $ null $ isNamed (fromString "{urn:xmpp:ping}ping") p = do
		log "urn:xmpp:ping" (from, to)
		writeStanzaChan toComponent $ iq {
			iqTo = Just from,
			iqFrom = Just to,
			iqType = IQResult,
			iqPayload = Nothing
		}
componentStanza _ _ _ _ _ toComponent _ (ReceivedIQ (iq@IQ { iqType = typ }))
	| typ `elem` [IQGet, IQSet] = do
		log "REPLY WITH IQ ERROR" iq
		writeStanzaChan toComponent $ iq {
			iqTo = iqFrom iq,
			iqFrom = iqTo iq,
			iqType = IQError,
			iqPayload = Just $ Element (fromString "{jabber:component:accept}error")
				[(fromString "{jabber:component:accept}type", [ContentText $ fromString "cancel"])]
				[NodeElement $ Element (fromString "{urn:ietf:params:xml:ns:xmpp-stanzas}feature-not-implemented") [] []]
		}
componentStanza _ _ _ _ _ _ _ s = log "UNKNOWN STANZA" s

participantJid payloads =
	listToMaybe $ mapMaybe (parseJID <=< attributeText (fromString "jid")) $
	isNamed (fromString "{http://jabber.org/protocol/muc#user}item") =<<
	elementChildren =<<
	isNamed (fromString "{http://jabber.org/protocol/muc#user}x") =<< payloads

component db toVitelity toRoomPresences toRejoinManager toJoinPartDebouncer toComponent componentHost = do
	thread <- forkXMPP $ forever $ flip catchError (log "component EXCEPTION") $ do
		stanza <- liftIO $ atomically $ readTChan toComponent
		log "COMPONENT OUT" stanza
		putStanza stanza

	flip catchError (\e -> liftIO (log "component part 2 EXCEPTION" e >> killThread thread)) $ forever $ do
		s <- getStanza
		log "COMPONENT  IN" s
		liftIO $ componentStanza db toVitelity toRoomPresences toRejoinManager toJoinPartDebouncer toComponent componentHost s

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

data Command = Help | Create Text | Join JID | JoinInvited | JoinInvitedWrong | Send Text | Who | List | Leave | InviteCmd JID | SetNick Text | Whisper JID Text | VitelityBogus Text
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
	| citxt == fromString "join" = Just JoinInvitedWrong
	| citxt == fromString "/leave" = Just Leave
	| citxt == fromString "/part" = Just Leave
	| citxt == fromString "/who" = Just Who
	| citxt == fromString "/list" = Just List
	| citxt == fromString "/help" = Just Help
	| citxt == fromString "You are not authorized to send SMS messages." = Just $ VitelityBogus txt
	| otherwise = Just $ Send txt
	where
	citxt = CI.mk txt

getMessage (ReceivedMessage m) = Just m
getMessage _ = Nothing

sendToRoom toComponent componentHost tel room msg = do
	log "SEND TO ROOM" (tel, room, msg)
	uuid <- (fmap.fmap) UUID.toString UUID.nextUUID
	writeStanzaChan toComponent $ (emptyMessage MessageGroupChat) {
		messageTo = parseJID $ bareTxt room,
		messageFrom = telToJid tel (fromString componentHost),
		messageID = Just $ fromString ("CHEOGRAM%" <> fromMaybe "UUIDFAIL" uuid),
		messagePayloads = [Element (fromString "{jabber:component:accept}body") [] [NodeContent $ ContentText msg]]
	}

leaveRoom db toComponent componentHost tel reason = do
	existingRoom <- tcGetJID db tel "joined"
	log "LEAVE ROOM" (existingRoom, tel, reason)
	forM_ existingRoom $ \leaveRoom -> do
		writeStanzaChan toComponent $ (emptyPresence PresenceUnavailable) {
			presenceTo = Just leaveRoom,
			presenceFrom = telToJid tel (fromString componentHost),
			presencePayloads = [Element (fromString "{jabber:component:accept}status") [] [NodeContent $ ContentText $ fromString reason]]
		}
		return ()

joinRoom db toComponent componentHost tel room =
	rejoinRoom db toComponent componentHost tel room False

rejoinRoom db toComponent componentHost tel room rejoin = do
	log "JOIN ROOM" (room, tel)
	password <- TC.runTCM $ TC.get db (tcKey tel (T.unpack (bareTxt room) <> "\0muc_roomsecret"))
	let pwEl = maybe [] (\pw -> [
			NodeElement $ Element (fromString "{http://jabber.org/protocol/muc}password") [] [NodeContent $ ContentText $ fromString pw]
		]) password

	uuid <- fromMaybe "UUIDFAIL" <$> (fmap.fmap) (fromString . UUID.toString) UUID.nextUUID
	writeStanzaChan toComponent $ (emptyPresence PresenceAvailable) {
		presenceID = Just $ fromString $ (if rejoin then "CHEOGRAMREJOIN%" else "") <> uuid,
		presenceTo = Just room,
		presenceFrom = telToJid tel (fromString componentHost),
		presencePayloads = [Element (fromString "{http://jabber.org/protocol/muc}x") [] ([
			NodeElement $ Element (fromString "{http://jabber.org/protocol/muc}history") [(fromString "{http://jabber.org/protocol/muc}maxchars", [ContentText $ fromString "0"])] []
		] <> pwEl)]
	}

addMUCOwner toComponent room from jid = do
	log "ADD MUC OWNER" (room, from, jid)
	uuid <- (fmap.fmap) UUID.toString UUID.nextUUID
	writeStanzaChan toComponent $ (emptyIQ IQSet) {
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
	}

createRoom :: TChan StanzaRec -> String -> [String] -> String -> String -> IO Bool
createRoom toComponent componentHost (server:otherServers) tel name = do
	log "START CREATE ROOM" (name, tel, server:otherServers)
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

sendInvite db toComponent to (Invite { inviteMUC = room, inviteFrom = from }) = do
	log "SEND INVITE" (room, to, from)
	membersonly <- maybe False toEnum <$> TC.runTCM (TC.get db (T.unpack (bareTxt room) <> "\0muc_membersonly"))
	when membersonly $
		-- Try to add everyone we invite as an owner also
		addMUCOwner toComponent room from to

	writeStanzaChan toComponent $ (emptyMessage MessageNormal) {
		messageTo = Just room,
		messageFrom = Just from,
		messagePayloads = [
			Element (fromString "{http://jabber.org/protocol/muc#user}x") [] [
				NodeElement $ Element (fromString "{http://jabber.org/protocol/muc#user}invite") [
					(fromString "{http://jabber.org/protocol/muc#user}to", [ContentText $ formatJID to])
				] []
			]
		]
	}

	writeStanzaChan toComponent $ (emptyMessage MessageNormal) {
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
		Just JoinInvitedWrong
			| Just room <- existingRoom -> sendToRoom toComponent componentHost tel room (fromString "Join")
			| otherwise -> do
				invitedRoom <- tcGetJID db tel "invited"
				let toJoin = invitedRoom >>= \jid -> parseJID (bareTxt jid <> fromString "/" <> nick)
				case toJoin of
					Just room -> do
						writeStanzaChan toVitelity $ mkSMS tel (fromString "I think you meant \"/join\", trying anyway...")
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
			let f = fst :: (String, Maybe String) -> String
			let snick = T.unpack nick
			let room = maybe "" (T.unpack . bareTxt) existingRoom
			presence <- fmap (fromMaybe [] . (readZ =<<)) (TC.runTCM $ TC.get db ("presence\0" <> room))
			let presence' = filter (/= snick) $ map f presence
			if null presence' then
				writeStanzaChan toVitelity $ mkSMS tel $ fromString $
					"You are not joined to a group. Reply with /help to learn more"
			else
				writeStanzaChan toVitelity $ mkSMS tel $ fromString $ mconcat [
					"You are joined to ", room,
					" as ", snick,
					" along with\n",
					intercalate ", " presence'
				]
		Just List -> do
			bookmarks <- fmap (fromMaybe [] . (readZ =<<)) (TC.runTCM $ TC.get db (tcKey tel "bookmarks"))
			writeStanzaChan toVitelity $ mkSMS tel $ fromString $ "Groups you can /join\n" <> intercalate "\n" bookmarks
		Just (InviteCmd jid)
			| Just room <- existingRoom, Just from <- telToJid tel (fromString componentHost) ->
				sendInvite db toComponent jid (Invite room from Nothing Nothing)
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
		Just (VitelityBogus txt) -> log "Bogus Vitelity message" txt
		Nothing -> writeStanzaChan toVitelity $ mkSMS tel (fromString "You sent an invalid message")

viteltiy db chunks toVitelity toComponent componentHost conferenceServers = do
	putStanza $ emptyPresence PresenceAvailable

	thread <- forkXMPP $ forever $ flip catchError (liftIO . log "vitelity EXCEPTION") $ do
		wait <- liftIO $ getStdRandom (randomR (1000000,2000000))
		stanza <- liftIO $ atomically $ readTChan toVitelity
		forM_ (strNode <$> (jidNode =<< stanzaTo stanza)) $ \tel -> do
			welcomed <- maybe False toEnum <$> liftIO (TC.runTCM $ TC.get db $ tcKey tel "welcomed")
			unless welcomed $ do
				putStanza $ mkSMS tel $ fromString "Welcome to CheoGram! You can chat with groups of friends (one at a time), by replying to this number. Reply with /help to learn more or visit cheogram.com"
				True <- liftIO (TC.runTCM $ TC.put db (tcKey tel "welcomed") (fromEnum True))
				liftIO $ threadDelay wait

		putStanza stanza
		log "VITELITY OUT" stanza
		liftIO $ threadDelay wait

	flip catchError (\e -> liftIO (log "viteltiy part 2 EXCEPTION" e >> killThread thread)) $ forever $ do
		m <- getMessage <$> getStanza
		mapM_ (log "VITELITY  IN") m
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

syncCall chan req = do
	var <- atomically $ newEmptyTMVar
	atomically $ writeTChan chan (req var)
	atomically $ takeTMVar var

data RejoinManagerCommand =
	CheckPings      |
	PingReply   JID |
	PingError   JID |
	Joined      JID |
	ForceRejoin JID Text

data RejoinManagerState = PingSent Text | Rejoining

rejoinManager db toComponent componentHost toRoomPresences toRejoinManager =
	next mempty
	where
	mkMucJid muc nick = parseJID $ bareTxt muc <> fromString "/" <> nick
	ourJids muc (x,y) = (,) <$> mkMucJid muc x <*> (T.stripSuffix (fromString $ "@" <> componentHost) =<< y)

	next state = atomically (readTChan toRejoinManager) >>= go state

	go state (PingReply mucJid) =
		next $! Map.delete mucJid state
	go state (PingError mucJid) = do
		forM_ (Map.lookup mucJid state) $ \x -> case x of
			PingSent tel -> atomically $ writeTChan toRejoinManager (ForceRejoin mucJid tel)
			_ -> return ()
		next state
	go state (Joined mucJid) =
		next $! Map.delete mucJid state
	go state (ForceRejoin mucJid tel) = do
		atomically $ writeTChan toRoomPresences (StartRejoin tel mucJid)
		rejoinRoom db toComponent componentHost tel mucJid True
		next $! Map.insert mucJid Rejoining state
	go state CheckPings = do
		presenceKeys <- TC.runTCM $ TC.fwmkeys db "presence\0" maxBound
		(next =<<) $! (\x -> foldM x state (presenceKeys :: [String])) $ \state pkey -> do
			let Just muc = parseJID =<< T.stripPrefix (fromString "presence\0") (T.pack pkey)
			putStrLn $ fromString "Checking (ping?) participants in " <> formatJID muc <> fromString "..."
			presences <- fmap (mapMaybe (ourJids muc) . fromMaybe [] . (readZ =<<)) (TC.runTCM $ TC.get db pkey)
			(\x -> foldM x state presences) $ \state (mucJid, tel) ->
				case Map.lookup mucJid state of
					Nothing -> do
						log "PINGING" (mucJid, tel)
						uuid <- fromMaybe "UUIDFAIL" <$> (fmap.fmap) (fromString . UUID.toString) UUID.nextUUID
						writeStanzaChan toComponent $ (emptyIQ IQGet) {
							iqTo = Just mucJid,
							iqFrom = parseJID $ tel <> T.pack ("@" <> componentHost),
							iqID = Just $ fromString $ "CHEOGRAMPING%" <> uuid,
							iqPayload = Just $ Element (fromString "{urn:xmpp:ping}ping") [] []
						}
						return $! Map.insert mucJid (PingSent tel) state
					Just (PingSent _) -> do -- Timeout, rejoin
						log "PING TIMEOUT" (mucJid, tel)
						atomically $ writeTChan toRejoinManager (ForceRejoin mucJid tel)
						return state
					Just Rejoining -> -- Don't ping, we're working on it
						return state

-- tel, from (bare is MUC, resource is nick), Maybe participantJID
data RoomPresences =
	RecordJoin Text JID (Maybe JID) |
	RecordPart Text JID |
	RecordNickChanged Text JID Text |
	Clear Text JID |
	StartRejoin Text JID |
	GetRoomPresences Text JID (TMVar [(String, Maybe String)])

roomPresences db toRoomPresences =
	forever $ atomically (readTChan toRoomPresences) >>= go
	where
	go (RecordJoin tel from jid) = do
		-- After a join is done we have a full presence list, remove old ones
		void $ TC.runTCM $ TC.out db $ tcKey tel (muc from <> "\0old_presence")
		globalAndLocal tel from ((resource from, T.unpack . bareTxt <$> jid):)
	go (RecordPart tel from) = do
		globalAndLocal tel from (filter ((/=resource from) . fst))
	go (RecordNickChanged tel from nick) =
		globalAndLocal tel from $
			map (first $ \n -> if fromString n == resource from then T.unpack nick else n)
	go (Clear tel from) =
		void $ TC.runTCM $ TC.out db $ tcKey tel (muc from <> "\0presence")
	go (StartRejoin tel from) = do
		-- Copy current presences to a holding space so we can clear when rejoin is over
		presences <- (fromMaybe [] . (readZ =<<)) <$>
			(TC.runTCM $ TC.get db $ tcKey tel (muc from <> "\0presence"))
		old_presences <- (fromMaybe [] . (readZ =<<)) <$>
			(TC.runTCM $ TC.get db $ tcKey tel (muc from <> "\0old_presence"))
		True <- TC.runTCM $ TC.put db (tcKey tel (muc from <> "\0old_presence"))
			(show (presences <> old_presences :: [(String, Maybe String)]))
		void $ TC.runTCM $ TC.out db $ tcKey tel (muc from <> "\0presence")
	go (GetRoomPresences tel from rtrn) = do
		presences <- (fromMaybe [] . (readZ =<<)) <$>
			(TC.runTCM $ TC.get db $ tcKey tel (muc from <> "\0presence"))
		old_presences <- (fromMaybe [] . (readZ =<<)) <$>
			(TC.runTCM $ TC.get db $ tcKey tel (muc from <> "\0old_presence"))
		atomically $ putTMVar rtrn $ sort $ nubBy (equating fst) $ presences <> old_presences

	globalAndLocal tel from f = do
		modify ("presence\0" <> muc from) f
		modify (tcKey tel (muc from <> "\0presence")) f
	modify :: String -> ([(String, Maybe String)] -> [(String, Maybe String)]) -> IO ()
	modify k f = do
		presence <- fmap (fromMaybe [] . (readZ =<<)) (TC.runTCM $ TC.get db k)
		True <- TC.runTCM $ TC.put db k $ show $ sort $ nubBy (equating fst) $ f presence
		return ()
	muc = T.unpack . bareTxt
	resource x = fromMaybe "" (T.unpack . strResource <$> jidResource x)

data JoinPartDebounce = DebounceJoin Text JID (Maybe JID) | DebouncePart Text JID | DebounceExpire Text JID UTCTime deriving (Show)

joinPartDebouncer toVitelity toRoomPresences toJoinPartDebouncer = next mempty
	where
	next state = do
		msg <- atomically (readTChan toJoinPartDebouncer)
		log "DEBOUNCE JOIN/PART" (msg, state)
		go state msg >>= next

	sendPart tel from time = do
		log "DEBOUNCE PART, GONNA SEND" (tel, from, time)
		atomically $ writeTChan toRoomPresences $ RecordPart tel from
		now <- getCurrentTime
		writeStanzaChan toVitelity $ mkSMS tel $ mconcat [
				fromString "* ",
				fromMaybe mempty (strResource <$> jidResource from),
				fromString " left the group ",
				fromString $ show $ round ((now `diffUTCTime` time) / 60),
				fromString " minutes ago"
			]

	sendJoin tel from time mjid = do
		let nick = fromMaybe mempty (strResource <$> jidResource from)
		presences <- syncCall toRoomPresences $ GetRoomPresences tel from
		now <- getCurrentTime
		log "DEBOUNCE JOIN, MAYBE GONNA SEND" (tel, from, presences)
		when (isNothing $ lookup (T.unpack nick) presences) $ do
			atomically $ writeTChan toRoomPresences $ RecordJoin tel from mjid
			writeStanzaChan toVitelity $ mkSMS tel $ mconcat [
					fromString "* ",
					nick,
					fromString " joined the group ",
					fromString $ show $ round ((now `diffUTCTime` time) / 60),
					fromString " minutes ago"
				]

	debounceCheck state tel from mjid join =
		case Map.lookup (tel, from) state of
			Just (_, _, j) | j /= join -> return $! Map.delete (tel, from) state -- debounce
			Just (_, _, _) -> return state -- ignore dupe
			Nothing -> do
				time <- getCurrentTime
				void $ forkIO $ threadDelay 120000000 >> atomically (writeTChan toJoinPartDebouncer $ DebounceExpire tel from time)
				return $! Map.insert (tel, from) (time, mjid, join) state

	go state (DebounceJoin tel from mjid) =
		debounceCheck state tel from mjid True
	go state (DebouncePart tel from) =
		debounceCheck state tel from Nothing False
	go state (DebounceExpire tel from time) =
		case Map.updateLookupWithKey (\_ (t,m,j) -> if t == time then Nothing else Just (t,m,j)) (tel, from) state of
			(Just (t, mjid, join), state')
				| t == time && join -> sendJoin tel from time mjid >> return state'
				| t == time -> sendPart tel from time >> return state'
			(_, state') -> return state'

openTokyoCabinet :: (TC.TCDB a) => String -> IO a
openTokyoCabinet pth = TC.runTCM $ do
	db <- TC.new
	True <- TC.open db pth [TC.OREADER, TC.OWRITER, TC.OCREAT]
	return db

main = do
	hSetBuffering stdout LineBuffering
	hSetBuffering stderr LineBuffering

	putStrLn $ fromString "Starting..."
	(name:host:port:secret:vitelityJid:vitelityPassword:conferences) <- getArgs
	db <- openTokyoCabinet "./db.tcdb" :: IO TC.HDB
	chunks <- atomically newTChan
	toJoinPartDebouncer <- atomically newTChan
	toVitelity <- atomically newTChan
	toComponent <- atomically newTChan
	toRoomPresences <- atomically newTChan
	toRejoinManager <- atomically newTChan

	void $ forkIO $ forever $ threadDelay 1500000 >> atomically (writeTChan chunks TimerExpire)
	void $ forkIO $ multipartStitcher db chunks toVitelity toComponent name conferences
	void $ forkIO $ joinPartDebouncer toVitelity toRoomPresences toJoinPartDebouncer
	void $ forkIO $ roomPresences db toRoomPresences

	void $ forkIO $ forever $ atomically (writeTChan toRejoinManager CheckPings) >> threadDelay 120000000
	void $ forkIO $ rejoinManager db toComponent name toRoomPresences toRejoinManager

	void $ forkIO $ forever $ log "runComponent ENDED" =<< (runEitherT . syncIO) (runComponent (Server (fromString name) host (PortNumber $ fromIntegral (read port :: Int))) (fromString secret) (component db toVitelity toRoomPresences toRejoinManager toJoinPartDebouncer toComponent name))

	let Just vitelityParsedJid = parseJID $ fromString vitelityJid
	forever $ runClient (Server (fromString "s.ms") "s.ms" (PortNumber 5222)) vitelityParsedJid (fromMaybe mempty $ strNode <$> jidNode vitelityParsedJid) (fromString vitelityPassword) $ do
		void $ bindJID vitelityParsedJid
		viteltiy db chunks toVitelity toComponent name conferences
