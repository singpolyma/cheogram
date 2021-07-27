module ConfigureDirectMessageRoute (main, nodeName) where

import Prelude ()
import BasicPrelude hiding (log)
import Data.Foldable (toList)
import Control.Concurrent
import Control.Concurrent.STM
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Data.XML.Types (Element(..), Node(NodeContent, NodeElement), Name(..), Content(ContentText), isNamed, hasAttributeText, elementText, elementChildren, attributeText)
import Control.Monad.Loops (iterateM_)

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as UUID (toString, fromString)
import qualified Data.UUID.V1 as UUID (nextUUID)
import qualified Network.Protocol.XMPP as XMPP
import qualified Data.Bool.HT as HT
import qualified Data.XML.Types as XML

import Util

newtype SessionID = SessionID UUID deriving (Ord, Eq, Show)

main :: XMPP.Domain -> (XMPP.JID -> IO (Maybe XMPP.JID)) -> (XMPP.JID -> Maybe XMPP.JID -> IO ()) -> IO (XMPP.IQ -> IO (Maybe XMPP.IQ))
main componentDomain getRouteJid setRouteJid = do
	stanzas <- newTQueueIO
	void $ forkIO $ iterateM_ (\sessions -> do
			(iq, reply) <- atomically (readTQueue stanzas)
			(sessions', response) <- processOneIQ componentDomain getRouteJid setRouteJid sessions iq
			atomically $ reply response
			now <- getCurrentTime
			return $! Map.filter (\(_, time) -> now `diffUTCTime` time < 600) sessions'
		) Map.empty
	return (\iq -> do
			result <- atomically newEmptyTMVar
			atomically $ writeTQueue stanzas (iq, putTMVar result)
			atomically $ readTMVar result
		)

processOneIQ :: XMPP.Domain -> (XMPP.JID -> IO (Maybe XMPP.JID)) -> (XMPP.JID -> Maybe XMPP.JID -> IO ()) -> Map SessionID (Session, UTCTime) -> XMPP.IQ -> IO (Map SessionID (Session, UTCTime), Maybe XMPP.IQ)
processOneIQ componentDomain getRouteJid setRouteJid sessions iq@(XMPP.IQ { XMPP.iqID = Just iqID, XMPP.iqFrom = Just from, XMPP.iqPayload = realPayload })
	| Just sid <- sessionIDFromText . snd =<< T.uncons =<< T.stripPrefix (s"ConfigureDirectMessageRoute") iqID,
          XMPP.iqType iq == XMPP.IQResult || XMPP.iqType iq == XMPP.IQError =
		(fmap Just) <$> lookupAndStepSession setRouteJid sessions componentDomain sid iqID from payload
	| elementName payload /= s"{http://jabber.org/protocol/commands}command" ||
	  attributeText (s"node") payload /= Just nodeName = do
		log "ConfigureDirectMessageRoute.processOneIQ BAD INPUT" (elementName payload, attributeText (s"node") payload)
		if XMPP.iqType iq == XMPP.IQError then
			return (sessions, Nothing)
		else
			return (sessions, Just $ iqError (Just iqID) (Just from) "cancel" "feature-not-implemented" Nothing)
	| Just sid <- sessionIDFromText =<< attributeText (s"sessionid") payload =
		(fmap Just) <$> lookupAndStepSession setRouteJid sessions componentDomain sid iqID from payload
	| otherwise = do
		(sid, session) <- newSession
		now <- getCurrentTime
		existingRoute <- getRouteJid from
		return (Map.insert sid (session, now) sessions, Just $ stage1 existingRoute from iqID sid)
	where
	payload
		| Just p <- realPayload,
		  XMPP.iqType iq == XMPP.IQError && elementName p == s"{jabber:component:accept}error" = p
		| XMPP.iqType iq == XMPP.IQError =
			let Just p = XMPP.iqPayload $ iqError Nothing Nothing "cancel" "internal-server-error" Nothing in p
		| otherwise = fromMaybe (Element (s"no-payload") [] []) realPayload
processOneIQ _ _ _ sessions iq@(XMPP.IQ { XMPP.iqID = iqID, XMPP.iqFrom = from }) = do
	log "ConfigureDirectMessageRoute.processOneIQ BAD INPUT" iq
	return (sessions, Just $ iqError iqID from "cancel" "feature-not-implemented" Nothing)

lookupAndStepSession :: (XMPP.JID -> Maybe XMPP.JID -> IO ()) -> Map SessionID (Session, UTCTime) -> Session' (IO (Map SessionID (Session, UTCTime), XMPP.IQ))
lookupAndStepSession setRouteJid sessions componentDomain sid iqID from payload
	| Just (stepSession, _) <- Map.lookup sid sessions =
		case attributeText (s"action") payload of
			Just action | action == s"cancel" ->
				return (Map.delete sid sessions, (XMPP.emptyIQ XMPP.IQResult) {
					XMPP.iqID = Just iqID,
					XMPP.iqTo = Just from,
					XMPP.iqPayload = Just $ Element (s"{http://jabber.org/protocol/commands}command")
						[
							(s"{http://jabber.org/protocol/commands}node", [ContentText nodeName]),
							(s"{http://jabber.org/protocol/commands}sessionid", [ContentText $ sessionIDToText sid]),
							(s"{http://jabber.org/protocol/commands}status", [ContentText $ s"cancelled"])
						] []
				})
			Just action | action == s"complete" ->
				return (Map.delete sid sessions, (XMPP.emptyIQ XMPP.IQResult) {
					XMPP.iqID = Just iqID,
					XMPP.iqTo = Just from,
					XMPP.iqPayload = Just $ Element (s"{http://jabber.org/protocol/commands}command")
						[
							(s"{http://jabber.org/protocol/commands}node", [ContentText nodeName]),
							(s"{http://jabber.org/protocol/commands}sessionid", [ContentText $ sessionIDToText sid]),
							(s"{http://jabber.org/protocol/commands}status", [ContentText $ s"completed"])
						] [
							NodeElement $ Element (s"{http://jabber.org/protocol/commands}note") [
								(s"{http://jabber.org/protocol/commands}type", [ContentText $ s"info"])
							] [
								NodeContent $ ContentText $ s"Saved route configuration."
							]
						]
				})
			_ ->
				let (session', iq) = stepSession componentDomain sid iqID from payload in
				fmap (flip (,) iq) $ case session' of
					SessionNext s -> do
						now <- getCurrentTime
						return $! Map.insert sid (s, now) sessions
					SessionCancel -> return $! Map.delete sid sessions
					SessionSaveAndNext userJid gatewayJid s -> do
						now <- getCurrentTime
						userJid `setRouteJid` (Just gatewayJid)
						return $! Map.insert sid (s, now) sessions
					SessionComplete userJid gatewayJid -> do
						userJid `setRouteJid` gatewayJid
						return $! Map.delete sid sessions
	| otherwise = do
		log "ConfigureDirectMessageRoute.processOneIQ NO SESSION FOUND" (sid, iqID, from, payload)
		return (sessions, iqError (Just iqID) (Just from) "modify" "bad-request" (Just "bad-sessionid"))

data SessionResult = SessionNext Session | SessionCancel | SessionSaveAndNext XMPP.JID XMPP.JID Session | SessionComplete XMPP.JID (Maybe XMPP.JID)
type Session' a = XMPP.Domain -> SessionID -> Text -> XMPP.JID -> Element -> a
type Session = Session' (SessionResult, XMPP.IQ)

data RegisterFormType = DataForm | LegacyRegistration

stage5 :: Text -> XMPP.JID -> Session
stage5 stage4iqID stage4from _ sid iqID from error
	| elementName error == s"{jabber:component:accept}error" =
		(SessionCancel, (XMPP.emptyIQ XMPP.IQError) {
			XMPP.iqID = Just stage4iqID,
			XMPP.iqTo = Just stage4from,
			XMPP.iqPayload = Just error
		})
	| otherwise =
		(SessionComplete stage4from (Just from), (XMPP.emptyIQ XMPP.IQResult) {
			XMPP.iqID = Just stage4iqID,
			XMPP.iqTo = Just stage4from,
			XMPP.iqPayload = Just $ Element (s"{http://jabber.org/protocol/commands}command")
				[
					(s"{http://jabber.org/protocol/commands}node", [ContentText nodeName]),
					(s"{http://jabber.org/protocol/commands}sessionid", [ContentText $ sessionIDToText sid]),
					(s"{http://jabber.org/protocol/commands}status", [ContentText $ s"completed"])
				]
				[
					NodeElement $ Element (s"{http://jabber.org/protocol/commands}note") [
						(s"{http://jabber.org/protocol/commands}type", [ContentText $ s"info"])
					] [
						NodeContent $ ContentText $ s"Registration complete."
					]
				]
		})

stage4 :: RegisterFormType -> XMPP.JID -> Session
stage4 formType gatewayJid componentDomain sid iqID from command
	| [form] <- isNamed (fromString "{jabber:x:data}x") =<< elementChildren command =
		(SessionNext $ stage5 iqID from, (XMPP.emptyIQ XMPP.IQSet) {
			XMPP.iqID = Just (s"ConfigureDirectMessageRoute4" ++ sessionIDToText sid),
			XMPP.iqTo = Just gatewayJid,
			XMPP.iqFrom = Just sendFrom, -- domain gets rewritten by main cheogram program
			XMPP.iqPayload = Just $
				case formType of
					DataForm -> Element (s"{jabber:iq:register}query") [] [NodeElement form]
					LegacyRegistration -> convertFormToLegacyRegistration form
		})
	| otherwise = (SessionCancel, iqError (Just iqID) (Just from) "modify" "bad-request" (Just "bad-payload"))
	where
	sendFrom = sendFromForBackend componentDomain from

stage3 :: Text -> XMPP.JID -> Session
stage3 stage2iqID stage2from _ sid iqID from query
	| elementName query == s"{jabber:component:accept}error" =
		(SessionCancel, (XMPP.emptyIQ XMPP.IQError) {
			XMPP.iqID = Just stage2iqID,
			XMPP.iqTo = Just stage2from,
			XMPP.iqPayload = Just query
		})
	| [form] <- isNamed (fromString "{jabber:x:data}x") =<< elementChildren query = processForm DataForm form
	| otherwise = processForm LegacyRegistration (convertQueryToForm query)
	where
	registered = not $ null $ isNamed (fromString "{jabber:iq:register}registered") =<< elementChildren query
	sessionNext
		| registered =
			SessionSaveAndNext stage2from from
		| otherwise = SessionNext
	processForm typ form =
		(sessionNext $ stage4 typ from, (XMPP.emptyIQ XMPP.IQResult) {
			XMPP.iqID = Just stage2iqID,
			XMPP.iqTo = Just stage2from,
			XMPP.iqPayload = Just $ commandStage sid registered form
		})

stage2 :: Session
stage2 componentDomain sid iqID from command
	| [form] <- isNamed (fromString "{jabber:x:data}x") =<< elementChildren command,
	  Just gatewayJid <- XMPP.parseJID =<< getFormField form (s"gateway-jid") =
		(
			SessionNext $ commandOrIBR gatewayJid,
			(queryCommandList' gatewayJid sendFrom) {
				XMPP.iqID = Just (s"ConfigureDirectMessageRoute2" ++ sessionIDToText sid)
			}
		)
	| [form] <- isNamed (fromString "{jabber:x:data}x") =<< elementChildren command,
	  getFormField form (s"gateway-jid") `elem` [Nothing, Just mempty] =
		(SessionComplete from Nothing, (XMPP.emptyIQ XMPP.IQResult) {
			XMPP.iqID = Just iqID,
			XMPP.iqTo = Just from,
			XMPP.iqPayload = Just $ Element (s"{http://jabber.org/protocol/commands}command")
				[
					(s"{http://jabber.org/protocol/commands}node", [ContentText nodeName]),
					(s"{http://jabber.org/protocol/commands}sessionid", [ContentText $ sessionIDToText sid]),
					(s"{http://jabber.org/protocol/commands}status", [ContentText $ s"completed"])
				]
				[
					NodeElement $ Element (s"{http://jabber.org/protocol/commands}note") [
						(s"{http://jabber.org/protocol/commands}type", [ContentText $ s"info"])
					] [
						NodeContent $ ContentText $ s"Direct message route removed."
					]
				]
		})
	| otherwise = (SessionCancel, iqError (Just iqID) (Just from) "modify" "bad-request" (Just "bad-payload"))
	where
	sendFrom = sendFromForBackend componentDomain from
	commandOrIBR gatewayJid _ _ _ _ command'
		| (s"jabber:iq:register") `elem` mapMaybe (attributeText (s"node")) (isNamed (s"{http://jabber.org/protocol/disco#items}item") =<< elementChildren command') =
			(SessionNext $ proxyAdHocFromGateway iqID from, (XMPP.emptyIQ XMPP.IQSet) {
				XMPP.iqID = Just (s"ConfigureDirectMessageRoute2" ++ sessionIDToText sid),
				XMPP.iqTo = Just gatewayJid,
				XMPP.iqFrom = Just sendFrom,
				XMPP.iqPayload = Just $ Element (s"{http://jabber.org/protocol/commands}command") [(s"node", [ContentText $ s"jabber:iq:register"])] []
			})
		| otherwise =
			(SessionNext $ stage3 iqID from, (XMPP.emptyIQ XMPP.IQGet) {
				XMPP.iqID = Just (s"ConfigureDirectMessageRoute2" ++ sessionIDToText sid),
				XMPP.iqTo = Just gatewayJid,
				XMPP.iqFrom = Just sendFrom, -- domain gets rewritten by main cheogram program
				XMPP.iqPayload = Just $ Element (s"{jabber:iq:register}query") [] []
			})

proxyAdHocFromGateway :: Text -> XMPP.JID -> Session
proxyAdHocFromGateway prevIqID userJid _ sid iqID from command
	| attributeText (s"status") command == Just (s"canceled") = (SessionCancel, proxied)
	| attributeText (s"status") command == Just (s"completed") =
		if (s"error") `elem` mapMaybe (attributeText (s"type")) (XML.isNamed (s"{http://jabber.org/protocol/commands}note") =<< XML.elementChildren command) then
			(SessionCancel, proxied)
		else
			(
				SessionComplete userJid (Just from),
				proxied {
					XMPP.iqPayload = fmap (\elem ->
						elem {
							XML.elementNodes = XML.elementNodes elem ++ [
								XML.NodeElement $ XML.Element (s"{http://jabber.org/protocol/commands}note")
									[(s"type", [XML.ContentText $ s"info"])]
									[XML.NodeContent $ XML.ContentText $ s"Registration complete."]
							]
						}
					) (XMPP.iqPayload proxied)
				}
			)
	| otherwise = (SessionNext $ proxyAdHocFromUser iqID otherSID from, proxied)
	where
	proxied =
		(XMPP.emptyIQ XMPP.IQResult) {
			XMPP.iqID = Just prevIqID,
			XMPP.iqTo = Just userJid,
			XMPP.iqPayload = Just $ command {
				XML.elementAttributes = map (\attr@(name, _) ->
					HT.select attr [
						(name == s"node", (name, [ContentText nodeName])),
						(name == s"sessionid", (name, [ContentText $ sessionIDToText sid]))
					]
				) (XML.elementAttributes command)
			}
		}
	otherSID = fromMaybe mempty $ XML.attributeText (s"sessionid") command

proxyAdHocFromUser :: Text -> Text -> XMPP.JID -> Session
proxyAdHocFromUser prevIqID otherSID gatewayJid componentDomain _ iqID from command = (
		SessionNext $ proxyAdHocFromGateway iqID from,
		(XMPP.emptyIQ XMPP.IQSet) {
			XMPP.iqID = Just prevIqID,
			XMPP.iqTo = Just gatewayJid,
			XMPP.iqFrom = Just sendFrom,
			XMPP.iqPayload = Just $ command {
				XML.elementAttributes = map (\attr@(name, _) ->
					HT.select attr [
						(name == s"node", (name, [s"jabber:iq:register"])),
						(name == s"sessionid", (name, [ContentText otherSID]))
					]
				) (XML.elementAttributes command)
			}
		}
	)
	where
	sendFrom = sendFromForBackend componentDomain from

stage1 :: Maybe XMPP.JID -> XMPP.JID -> Text -> SessionID -> XMPP.IQ
stage1 existingRoute iqTo iqID sid = (XMPP.emptyIQ XMPP.IQResult) {
	XMPP.iqTo = Just iqTo,
	XMPP.iqID = Just iqID,
	XMPP.iqPayload = Just $ commandStage sid False $
		Element (fromString "{jabber:x:data}x") [
			(fromString "{jabber:x:data}type", [ContentText $ s"form"])
		] [
			NodeElement $ Element (fromString "{jabber:x:data}title") [] [NodeContent $ ContentText $ s"Configure Direct Message Route"],
			NodeElement $ Element (fromString "{jabber:x:data}instructions") [] [
				NodeContent $ ContentText $ s"Enter the JID of a gateway to use for routing your direct messages over SMS."
			],
			NodeElement $ Element (fromString "{jabber:x:data}field") [
				(fromString "{jabber:x:data}type", [ContentText $ s"jid-single"]),
				(fromString "{jabber:x:data}var", [ContentText $ s"gateway-jid"]),
				(fromString "{jabber:x:data}label", [ContentText $ s"Gateway JID"])
			] [
				NodeElement $ Element (fromString "{jabber:x:data}value") [] [NodeContent $ ContentText $ maybe mempty XMPP.formatJID existingRoute]
			]
		]
}

sendFromForBackend :: XMPP.Domain -> XMPP.JID -> XMPP.JID
sendFromForBackend componentDomain from
	| XMPP.jidDomain from == componentDomain = from
	| otherwise = sendFrom
	where
	Just sendFrom = XMPP.parseJID $ (escapeJid $ bareTxt from) ++ s"@" ++ XMPP.strDomain componentDomain

commandStage :: SessionID -> Bool -> Element -> Element
commandStage sid allowComplete el = Element (s"{http://jabber.org/protocol/commands}command")
	[
		(s"{http://jabber.org/protocol/commands}node", [ContentText nodeName]),
		(s"{http://jabber.org/protocol/commands}sessionid", [ContentText $ sessionIDToText sid]),
		(s"{http://jabber.org/protocol/commands}status", [ContentText $ s"executing"])
	]
	[
		NodeElement actions,
		NodeElement el
	]
	where
	actions
		| allowComplete =
			Element (s"{http://jabber.org/protocol/commands}actions") [
				(s"{http://jabber.org/protocol/commands}execute", [ContentText $ s"complete"])
			] [
				NodeElement $ Element (s"{http://jabber.org/protocol/commands}next") [] [],
				NodeElement $ Element (s"{http://jabber.org/protocol/commands}complete") [] []
			]
		| otherwise =
			Element (s"{http://jabber.org/protocol/commands}actions") [
				(s"{http://jabber.org/protocol/commands}execute", [ContentText $ s"next"])
			] [
				NodeElement $ Element (s"{http://jabber.org/protocol/commands}next") [] []
			]

newSession :: IO (SessionID, Session)
newSession = UUID.nextUUID >>= go
	where
	go (Just uuid) = return (SessionID uuid, stage2)
	go Nothing = do
		log "ConfigureDirectMessageRoute.newSession" "UUID generation failed"
		UUID.nextUUID >>= go

sessionIDFromText :: Text -> Maybe SessionID
sessionIDFromText txt = SessionID <$> UUID.fromString (textToString txt)

sessionIDToText :: SessionID -> Text
sessionIDToText (SessionID uuid) = fromString $ UUID.toString uuid

nodeName :: Text
nodeName = s"register"

iqError :: Maybe Text -> Maybe XMPP.JID -> String -> String -> Maybe String -> XMPP.IQ
iqError iqID to typ xmpp command = (XMPP.emptyIQ XMPP.IQError) {
	XMPP.iqID = iqID,
	XMPP.iqTo = to,
	XMPP.iqPayload = Just $
		Element (s"{jabber:component:accept}error")
			[(s"{jabber:component:accept}type", [ContentText $ fromString typ])]
			(
				(NodeElement $ Element (fromString $ "{urn:ietf:params:xml:ns:xmpp-stanzas}" ++ xmpp) [] []) :
				map (\name ->
					NodeElement $ Element (fromString $ "{http://jabber.org/protocol/commands}" ++ name) [] []
				) (toList command)
			)
}

convertFormToLegacyRegistration :: Element -> Element
convertFormToLegacyRegistration form =
	Element (s"{jabber:iq:register}query") [] $
	map (NodeElement . uncurry legacyEl . varAndValue) fields
	where
	legacyEl var value = Element (fromString $ "{jabber:iq:register}" ++ T.unpack var) [] [NodeContent $ ContentText value]
	varAndValue field = (
			fromMaybe mempty $ attributeText (s"var") field,
			mconcat $ elementText =<< isNamed (s"{jabber:x:data}value") =<< elementChildren field
		)
	fields = isNamed (s"{jabber:x:data}field") =<< elementChildren form

convertQueryToForm :: Element -> Element
convertQueryToForm query =
	Element (s"{jabber:x:data}x") [
			(s"{jabber:x:data}type", [ContentText $ s"form"])
		] ([
			NodeElement $ Element (fromString "{jabber:x:data}title") [] [NodeContent $ ContentText $ s"Register"],
			NodeElement $ Element (fromString "{jabber:x:data}instructions") [] [NodeContent $ ContentText instructions]
		] ++ map (NodeElement . uncurry field) vars)
	where
	field var text =
		Element (fromString "{jabber:x:data}field") [
			(s"{jabber:x:data}type", [ContentText $ if var == s"password" then s"text-private" else s"text-single"]),
			(s"{jabber:x:data}var", [ContentText var]),
			(s"{jabber:x:data}label", [ContentText var])
		] [
			NodeElement $ Element (fromString "{jabber:x:data}value") [] [NodeContent $ ContentText text]
		]
	instructions = mconcat $ elementText =<< isNamed (s"{jabber:iq:register}instructions") =<< elementChildren query
	vars =
		map snd $
		filter (\(ns, (var, _)) -> ns == s"jabber:iq:register" && var `notElem` [s"registered", s"instructions"]) $
		mapMaybe (\el -> let name = elementName el in (,) <$> nameNamespace name <*> pure (nameLocalName name, mconcat $ elementText el)) $
		elementChildren query
