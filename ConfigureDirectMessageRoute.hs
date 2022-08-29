module ConfigureDirectMessageRoute (main, nodeName, switchBackendNodeName) where

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

switchBackendNodeName :: Text
switchBackendNodeName = s"https://ns.cheogram.com/sgx/jid-switch"

newtype SessionID = SessionID UUID deriving (Ord, Eq, Show)

type GetPossibleRoute = XMPP.JID -> IO (Maybe XMPP.JID)
type GetPossibleSwitch = XMPP.JID -> IO (Maybe (XMPP.JID, XMPP.JID, XMPP.JID))
type GetRouteJid = XMPP.JID -> IO (Maybe XMPP.JID)
type SetRouteJid = XMPP.JID -> Maybe XMPP.JID -> IO ()
type ClearSwitch = XMPP.JID -> IO ()

main :: XMPP.Domain -> GetPossibleRoute -> GetPossibleSwitch -> GetRouteJid -> SetRouteJid -> ClearSwitch -> IO (XMPP.IQ -> IO (Maybe XMPP.IQ))
main componentDomain getPossibleRoute getPossibleSwitch getRouteJid setRouteJid clearSwitch = do
	stanzas <- newTQueueIO
	void $ flip forkFinally (log "ConfigureDirectMessageRouteTOP") $ void $ iterateM_ (\sessions -> do
			(iq, reply) <- atomically (readTQueue stanzas)
			(sessions', response) <- processOneIQ componentDomain getPossibleRoute getPossibleSwitch getRouteJid setRouteJid clearSwitch sessions iq
			atomically $ reply response
			now <- getCurrentTime
			return $! Map.filter (\(_, time) -> now `diffUTCTime` time < 600) sessions'
		) Map.empty
	return (\iq -> do
			result <- atomically newEmptyTMVar
			atomically $ writeTQueue stanzas (iq, putTMVar result)
			atomically $ readTMVar result
		)

processOneIQ :: XMPP.Domain -> GetPossibleRoute -> GetPossibleSwitch -> GetRouteJid -> SetRouteJid -> ClearSwitch -> Map SessionID (Session, UTCTime) -> XMPP.IQ -> IO (Map SessionID (Session, UTCTime), Maybe XMPP.IQ)
processOneIQ componentDomain getPossibleRoute getPossibleSwitch getRouteJid setRouteJid clearSwitch sessions iq@(XMPP.IQ { XMPP.iqID = Just iqID, XMPP.iqFrom = Just from, XMPP.iqPayload = realPayload })
	| Just sid <- sessionIDFromText . snd =<< T.uncons =<< T.stripPrefix (s"ConfigureDirectMessageRoute") iqID,
          XMPP.iqType iq == XMPP.IQResult || XMPP.iqType iq == XMPP.IQError =
		(fmap Just) <$> lookupAndStepSession setRouteJid clearSwitch sessions componentDomain sid iqID from payload
	| elementName payload /= s"{http://jabber.org/protocol/commands}command" ||
	  attributeText (s"node") payload /= Just nodeName = do
		log "ConfigureDirectMessageRoute.processOneIQ BAD INPUT" (elementName payload, attributeText (s"node") payload)
		if XMPP.iqType iq == XMPP.IQError then
			return (sessions, Nothing)
		else
			return (sessions, Just $ iqError (Just iqID) (Just from) "cancel" "feature-not-implemented" Nothing)
	| Just sid <- sessionIDFromText =<< attributeText (s"sessionid") payload =
		(fmap Just) <$> lookupAndStepSession setRouteJid clearSwitch sessions componentDomain sid iqID from payload
	| otherwise = do
		now <- getCurrentTime
		existingRoute <- getRouteJid from
		possibleRoute <- getPossibleRoute from
		possibleSwitch <- getPossibleSwitch from
		case possibleSwitch of
			Just (newJid, switchJid, switchRoute) -> do
				(sid, session) <- newSession $ switchStage2 switchJid switchRoute possibleRoute existingRoute
				return (Map.insert sid (session, now) sessions, Just $ switchStage1 newJid switchJid switchRoute possibleRoute existingRoute from iqID sid)
			_ -> do
				(sid, session) <- newSession stage2
				return (Map.insert sid (session, now) sessions, Just $ stage1 possibleRoute existingRoute from iqID sid)
	where
	payload
		| Just p <- realPayload,
		  XMPP.iqType iq == XMPP.IQError && elementName p == s"{jabber:component:accept}error" = p
		| XMPP.iqType iq == XMPP.IQError =
			let Just p = XMPP.iqPayload $ iqError Nothing Nothing "cancel" "internal-server-error" Nothing in p
		| otherwise = fromMaybe (Element (s"no-payload") [] []) realPayload
processOneIQ _ _ _ _ _ _ sessions iq@(XMPP.IQ { XMPP.iqID = iqID, XMPP.iqFrom = from }) = do
	log "ConfigureDirectMessageRoute.processOneIQ BAD INPUT" iq
	return (sessions, Just $ iqError iqID from "cancel" "feature-not-implemented" Nothing)

lookupAndStepSession :: SetRouteJid -> ClearSwitch -> Map SessionID (Session, UTCTime) -> Session' (IO (Map SessionID (Session, UTCTime), XMPP.IQ))
lookupAndStepSession setRouteJid clearSwitch sessions componentDomain sid iqID from payload
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
							(s"{http://jabber.org/protocol/commands}status", [ContentText $ s"canceled"])
						] [
							NodeElement $ Element (s"{http://jabber.org/protocol/commands}note") [
								(s"{http://jabber.org/protocol/commands}type", [ContentText $ s"info"])
							] [
								NodeContent $ ContentText $ s"Register cancelled"
							]
						]
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
					SessionClearSwitchAndNext userJid s -> do
						now <- getCurrentTime
						clearSwitch userJid
						return $! Map.insert sid (s, now) sessions
					SessionCompleteSwitch userJid oldJid gatewayJid -> do
						userJid `setRouteJid` Just gatewayJid
						oldJid `setRouteJid` Nothing
						clearSwitch userJid
						return $! Map.delete sid sessions
					SessionComplete userJid gatewayJid -> do
						userJid `setRouteJid` gatewayJid
						return $! Map.delete sid sessions
	| otherwise = do
		log "ConfigureDirectMessageRoute.processOneIQ NO SESSION FOUND" (sid, iqID, from, payload)
		return (sessions, iqError (Just iqID) (Just from) "modify" "bad-request" (Just "bad-sessionid"))

data SessionResult = SessionNext Session | SessionCancel | SessionSaveAndNext XMPP.JID XMPP.JID Session | SessionClearSwitchAndNext XMPP.JID Session | SessionCompleteSwitch XMPP.JID XMPP.JID XMPP.JID | SessionComplete XMPP.JID (Maybe XMPP.JID)
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
	  Just gatewayJid <- XMPP.parseJID =<< getFormField form (s"gateway-jid"),
	  XMPP.jidNode gatewayJid == Nothing && XMPP.jidResource gatewayJid == Nothing =
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

-- Use SessionNext and SessionSaveAndNext to allow the proxied session to continue for prev
-- Rely on expiry for cleanup
proxyAdHocFromGateway :: Text -> XMPP.JID -> Session
proxyAdHocFromGateway prevIqID userJid _ sid iqID from command
	| attributeText (s"status") command == Just (s"canceled") = (SessionNext next, proxied)
	| attributeText (s"status") command == Just (s"completed") =
		if (s"error") `elem` mapMaybe (attributeText (s"type")) (XML.isNamed (s"{http://jabber.org/protocol/commands}note") =<< XML.elementChildren command) then
			(SessionNext next, proxied)
		else
			(
				SessionSaveAndNext userJid from next,
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
	| otherwise = (SessionNext next, proxied)
	where
	next = proxyAdHocFromUser iqID otherSID from
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

switchStage1 :: XMPP.JID -> XMPP.JID -> XMPP.JID -> Maybe XMPP.JID -> Maybe XMPP.JID -> XMPP.JID -> Text -> SessionID -> XMPP.IQ
switchStage1 newJid switchJid switchRoute possibleRoute existingRoute iqTo iqID sid = (XMPP.emptyIQ XMPP.IQResult) {
	XMPP.iqTo = Just iqTo,
	XMPP.iqID = Just iqID,
	XMPP.iqPayload = Just $ commandStage sid False $
		Element (fromString "{jabber:x:data}x") [
			(fromString "{jabber:x:data}type", [ContentText $ s"form"])
		] [
			NodeElement $ Element (fromString "{jabber:x:data}title") [] [NodeContent $ ContentText $ s"Accept Jabber ID Change"],
			NodeElement $ Element (fromString "{jabber:x:data}instructions") [] [
				NodeContent $ ContentText $ concat [
					s"It appears that the Jabber ID \"",
					bareTxt switchJid,
					s"\" has requested a migration to this Jabber ID (",
					bareTxt newJid,
					s"). If this isn't expected, respond no to the following to register normally"
				]
			],
			NodeElement $ Element (fromString "{jabber:x:data}field") [
				(fromString "{jabber:x:data}type", [ContentText $ s"boolean"]),
				(fromString "{jabber:x:data}var", [ContentText $ s"confirm"]),
				(fromString "{jabber:x:data}label", [ContentText $ s"Do you accept the migration?"])
			] []
		]
}

switchStage2 :: XMPP.JID -> XMPP.JID -> Maybe XMPP.JID -> Maybe XMPP.JID -> Session
switchStage2 switchJid switchRoute possibleRoute existingRoute componentDomain sid iqID from command
	| [form] <- isNamed (fromString "{jabber:x:data}x") =<< elementChildren command,
	  Just True <- parseBool =<< getFormField form (s"confirm") =
		(
			SessionNext $ switchStage3 switchJid switchRoute iqID from,
			(XMPP.emptyIQ XMPP.IQSet) {
				XMPP.iqID = Just (s"ConfigureDirectMessageRoute2" ++ sessionIDToText sid),
				XMPP.iqTo = Just switchRoute,
				XMPP.iqFrom = Just $ sendFromForBackend componentDomain switchJid,
				XMPP.iqPayload = Just $ Element (s"{http://jabber.org/protocol/commands}command") [(s"node", [ContentText switchBackendNodeName])] []
			}
		)
	| otherwise =
		(
			SessionClearSwitchAndNext from stage2,
			stage1 possibleRoute existingRoute from iqID sid
		)

switchStage3 :: XMPP.JID -> XMPP.JID -> Text -> XMPP.JID -> Session
switchStage3 switchJid switchRoute stage2ID stage2From componentDomain sid iqID from command
	| Just backendSid <- attributeText (s"sessionid") command,
	  [form] <- isNamed (fromString "{jabber:x:data}x") =<< elementChildren command,
	  isJust $ getFormField form $ s"jid" =
		(
			SessionNext $ switchStage4 switchJid switchRoute stage2ID stage2From,
			(XMPP.emptyIQ XMPP.IQSet) {
				XMPP.iqTo = Just from,
				XMPP.iqFrom = Just $ sendFromForBackend componentDomain switchJid,
				XMPP.iqID = Just (s"ConfigureDirectMessageRoute3" ++ sessionIDToText sid),
				XMPP.iqPayload = Just $ Element (s"{http://jabber.org/protocol/commands}command") [
						(s"node", [ContentText switchBackendNodeName]),
						(s"sessionid", [ContentText $ backendSid])
					] [
						NodeElement $ Element (fromString "{jabber:x:data}x") [
							(fromString "{jabber:x:data}type", [ContentText $ s"submit"])
						] [
							NodeElement $ Element (fromString "{jabber:x:data}field") [
								(fromString "{jabber:x:data}var", [ContentText $ s"jid"])
							] [
								NodeElement $ Element (fromString "{jabber:x:data}value") [] [NodeContent $ ContentText $ bareTxt stage2From]
							]
						]
					]
			}
			)
	| otherwise = (SessionCancel, iqError (Just stage2ID) (Just stage2From) "cancel" "internal-server-error" Nothing)

switchStage4 :: XMPP.JID -> XMPP.JID -> Text -> XMPP.JID -> Session
switchStage4 switchJid switchRoute stage2ID stage2From componentDomain sid iqID from command
	| attributeText (s"status") command == Just (s"canceled") = (SessionCancel, proxied)
	| attributeText (s"status") command == Just (s"completed") =
		if (s"error") `elem` mapMaybe (attributeText (s"type")) (XML.isNamed (s"{http://jabber.org/protocol/commands}note") =<< XML.elementChildren command) then
			(SessionCancel, proxied)
		else
			(SessionCompleteSwitch stage2From switchJid switchRoute, proxied)
	where
	proxied =
		(XMPP.emptyIQ XMPP.IQResult) {
			XMPP.iqID = Just stage2ID,
			XMPP.iqTo = Just stage2From,
			XMPP.iqPayload = Just $ command {
				XML.elementAttributes = map (\attr@(name, _) ->
					HT.select attr [
						(name == s"node", (name, [ContentText nodeName])),
						(name == s"sessionid", (name, [ContentText $ sessionIDToText sid]))
					]
				) (XML.elementAttributes command)
			}
		}

stage1 :: Maybe XMPP.JID -> Maybe XMPP.JID -> XMPP.JID -> Text -> SessionID -> XMPP.IQ
stage1 possibleRoute existingRoute iqTo iqID sid = (XMPP.emptyIQ XMPP.IQResult) {
	XMPP.iqTo = Just iqTo,
	XMPP.iqID = Just iqID,
	XMPP.iqPayload = Just $ commandStage sid False $
		Element (fromString "{jabber:x:data}x") [
			(s"{jabber:x:data}type", [s"form"])
		] (catMaybes [
			Just $ NodeElement $ Element (s"{jabber:x:data}title") [] [NodeContent $ s"Configure Direct Message Route"],
			Just $ NodeElement $ Element (s"{jabber:x:data}instructions") [] [
				NodeContent $ ContentText $ s"Enter the gateway to use for routing your direct messages over SMS."
			],
			flip fmap possibleRoute $ \route -> NodeElement $ Element (s"{jabber:x:data}instructions") [] [
				NodeContent $ ContentText $ s"To continue your registration with " ++ XMPP.formatJID route ++ s" please enter " ++ XMPP.formatJID route
			],
			Just $ NodeElement $ Element (s"{jabber:x:data}field") [
				(s"{jabber:x:data}type", [s"list-single"]),
				(s"{jabber:x:data}var", [s"gateway-jid"]),
				(s"{jabber:x:data}label", [s"Gateway"])
			] [
				NodeElement $ Element (s"{jabber:x:data}value") [] [NodeContent $ ContentText $ maybe mempty XMPP.formatJID existingRoute],
				NodeElement $ Element (s"{http://jabber.org/protocol/xdata-validate}validate")
					[(s"datatype", [s"xs:string"])]
					[NodeElement $ Element (s"{http://jabber.org/protocol/xdata-validate}open") [] []],
				NodeElement $ Element (s"{jabber:x:data}option")
					[(s"label", [s"JMP"])]
					[NodeElement $ Element (s"{jabber:x:data}value") [] [NodeContent $ s"jmp.chat"]],
				NodeElement $ Element (s"{jabber:x:data}option")
					[(s"label", [s"Vonage SGX"])]
					[NodeElement $ Element (s"{jabber:x:data}value") [] [NodeContent $ s"vonage.sgx.soprani.ca"]],
				NodeElement $ Element (s"{jabber:x:data}option")
					[(s"label", [s"Twilio SGX"])]
					[NodeElement $ Element (s"{jabber:x:data}value") [] [NodeContent $ s"twilio.sgx.soprani.ca"]]
			]
		])
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

newSession :: Session -> IO (SessionID, Session)
newSession nextStage = UUID.nextUUID >>= go
	where
	go (Just uuid) = return (SessionID uuid, nextStage)
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
