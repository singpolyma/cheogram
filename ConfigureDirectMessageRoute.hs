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

import Util

newtype SessionID = SessionID UUID deriving (Ord, Eq, Show)

main :: (XMPP.JID -> XMPP.JID -> IO ()) -> IO (XMPP.IQ -> IO XMPP.IQ)
main setRouteJid = do
	stanzas <- newTQueueIO
	void $ forkIO $ iterateM_ (\sessions -> do
			(iq, reply) <- atomically (readTQueue stanzas)
			(sessions', response) <- processOneIQ setRouteJid sessions iq
			atomically $ reply response
			now <- getCurrentTime
			return $! Map.filter (\(_, time) -> now `diffUTCTime` time < 600) sessions'
		) Map.empty
	return (\iq -> do
			result <- atomically newEmptyTMVar
			atomically $ writeTQueue stanzas (iq, putTMVar result)
			atomically $ readTMVar result
		)

processOneIQ :: (XMPP.JID -> XMPP.JID -> IO ()) -> Map SessionID (Session, UTCTime) -> XMPP.IQ -> IO (Map SessionID (Session, UTCTime), XMPP.IQ)
processOneIQ setRouteJid sessions iq@(XMPP.IQ { XMPP.iqID = Just iqID, XMPP.iqFrom = Just from, XMPP.iqPayload = realPayload })
	| Just sid <- sessionIDFromText . snd =<< T.uncons =<< T.stripPrefix (s"ConfigureDirectMessageRoute") iqID,
          XMPP.iqType iq == XMPP.IQResult || XMPP.iqType iq == XMPP.IQError =
		lookupAndStepSession setRouteJid sessions sid iqID from payload
	| elementName payload /= s"{http://jabber.org/protocol/commands}command" ||
	  attributeText (s"node") payload /= Just nodeName = do
		log "ConfigureDirectMessageRoute.processOneIQ BAD INPUT" (elementName payload, attributeText (s"node") payload)
		return (sessions, iqError (Just iqID) (Just from) "cancel" "feature-not-implemented" Nothing)
	| Just sid <- sessionIDFromText =<< attributeText (s"sessionid") payload =
		lookupAndStepSession setRouteJid sessions sid iqID from payload
	| otherwise = do
		(sid, session) <- newSession
		now <- getCurrentTime
		return (Map.insert sid (session, now) sessions, stage1 from iqID sid)
	where
	payload = fromMaybe (Element (s"no-payload") [] []) realPayload
processOneIQ _ sessions iq@(XMPP.IQ { XMPP.iqID = iqID, XMPP.iqFrom = from }) = do
	log "ConfigureDirectMessageRoute.processOneIQ BAD INPUT" iq
	return (sessions, iqError iqID from "cancel" "feature-not-implemented" Nothing)

lookupAndStepSession :: (XMPP.JID -> XMPP.JID -> IO ()) -> Map SessionID (Session, UTCTime) -> Session' (IO (Map SessionID (Session, UTCTime), XMPP.IQ))
lookupAndStepSession setRouteJid sessions sid iqID from payload
	| Just (stepSession, _) <- Map.lookup sid sessions =
		if attributeText (s"{http://jabber.org/protocol/commands}action") payload == Just (s"cancel") then
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
		else
			let (session', iq) = stepSession sid iqID from payload in
			fmap (flip (,) iq) $ case session' of
				SessionNext s -> do
					now <- getCurrentTime
					return $! Map.insert sid (s, now) sessions
				SessionCancel -> return $! Map.delete sid sessions
				SessionComplete userJid gatewayJid -> do
					userJid `setRouteJid` gatewayJid
					return $! Map.delete sid sessions
	| otherwise = do
		log "ConfigureDirectMessageRoute.processOneIQ NO SESSION FOUND" (sid, iqID, from, payload)
		return (sessions, iqError (Just iqID) (Just from) "modify" "bad-request" (Just "bad-sessionid"))

data SessionResult = SessionNext Session | SessionCancel | SessionComplete XMPP.JID XMPP.JID
type Session' a = SessionID -> Text -> XMPP.JID -> Element -> a
type Session = Session' (SessionResult, XMPP.IQ)

data RegisterFormType = DataForm | LegacyRegistration

stage5 :: Text -> XMPP.JID -> Session
stage5 stage4iqID stage4from sid iqID from error
	| elementName error == s"{jabber:component:accept}error" =
		(SessionCancel, (XMPP.emptyIQ XMPP.IQError) {
			XMPP.iqID = Just stage4iqID,
			XMPP.iqTo = Just stage4from,
			XMPP.iqPayload = Just error
		})
	| otherwise =
		(SessionComplete stage4from from, (XMPP.emptyIQ XMPP.IQResult) {
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
stage4 formType gatewayJid sid iqID from command
	| [form] <- isNamed (fromString "{jabber:x:data}x") =<< elementChildren command,
	  Just sendFrom <- XMPP.parseJID $ (escapeJid $ bareTxt from) ++ s"@cheogram" =
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

stage3 :: Text -> XMPP.JID -> Session
stage3 stage2iqID stage2from sid iqID from query
	| elementName query == s"{jabber:component:accept}error" =
		(SessionCancel, (XMPP.emptyIQ XMPP.IQError) {
			XMPP.iqID = Just stage2iqID,
			XMPP.iqTo = Just stage2from,
			XMPP.iqPayload = Just query
		})
	| [form] <- isNamed (fromString "{jabber:x:data}x") =<< elementChildren query = processForm DataForm form
	| otherwise = processForm LegacyRegistration (convertQueryToForm query)
	where
	processForm typ form =
		(SessionNext $ stage4 typ from, (XMPP.emptyIQ XMPP.IQResult) {
			XMPP.iqID = Just stage2iqID,
			XMPP.iqTo = Just stage2from,
			XMPP.iqPayload = Just $ commandStage sid form
		})

stage2 :: Session
stage2 sid iqID from command
	| [form] <- isNamed (fromString "{jabber:x:data}x") =<< elementChildren command,
	  Just gatewayJid <- XMPP.parseJID =<< getFormField form (s"gateway-jid") =
		(SessionNext $ stage3 iqID from, (XMPP.emptyIQ XMPP.IQGet) {
			XMPP.iqID = Just (s"ConfigureDirectMessageRoute2" ++ sessionIDToText sid),
			XMPP.iqTo = Just gatewayJid,
			XMPP.iqPayload = Just $ Element (s"{jabber:iq:register}query") [] []
		})
	| otherwise = (SessionCancel, iqError (Just iqID) (Just from) "modify" "bad-request" (Just "bad-payload"))

stage1 :: XMPP.JID -> Text -> SessionID -> XMPP.IQ
stage1 iqTo iqID sid = (XMPP.emptyIQ XMPP.IQResult) {
	XMPP.iqTo = Just iqTo,
	XMPP.iqID = Just iqID,
	XMPP.iqPayload = Just $ commandStage sid $
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
			] []
		]
}

commandStage :: SessionID -> Element -> Element
commandStage sid el = Element (s"{http://jabber.org/protocol/commands}command")
	[
		(s"{http://jabber.org/protocol/commands}node", [ContentText nodeName]),
		(s"{http://jabber.org/protocol/commands}sessionid", [ContentText $ sessionIDToText sid]),
		(s"{http://jabber.org/protocol/commands}status", [ContentText $ s"executing"])
	]
	[
		NodeElement $ Element (s"{http://jabber.org/protocol/commands}actions") [
			(s"{http://jabber.org/protocol/commands}execute", [ContentText $ s"next"])
		] [
			NodeElement $ Element (s"{http://jabber.org/protocol/commands}next") [] []
		],
		NodeElement el
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
nodeName = s"configure-direct-message-route"

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
		] ++ map (NodeElement . field) vars)
	where
	field var =
		Element (fromString "{jabber:x:data}field") [
			(s"{jabber:x:data}type", [ContentText $ if var == s"password" then s"text-private" else s"text-single"]),
			(s"{jabber:x:data}var", [ContentText var]),
			(s"{jabber:x:data}label", [ContentText var])
		] []
	instructions = mconcat $ elementText =<< isNamed (s"{jabber:iq:register}instructions") =<< elementChildren query
	vars =
		map snd $
		filter (\(ns, var) -> ns == s"jabber:iq:register" && var `notElem` [s"registered", s"instructions"]) $
		mapMaybe (\el -> let name = elementName el in (,) <$> nameNamespace name <*> pure (nameLocalName name)) $
		elementChildren query
