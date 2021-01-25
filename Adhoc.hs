module Adhoc(adhocBotSession, commandList, queryCommandList) where

import Prelude ()
import BasicPrelude hiding (log)
import Control.Concurrent (myThreadId, killThread)
import Control.Concurrent.STM
import Control.Error (hush)
import Data.XML.Types as XML (Element(..), Node(NodeContent, NodeElement), Content(ContentText), isNamed, elementText, elementChildren, attributeText)

import Network.Protocol.XMPP (JID(..), parseJID, formatJID, IQ(..), IQType(..), emptyIQ, Message(..))
import qualified Network.Protocol.XMPP as XMPP

import qualified Control.Concurrent.STM.Delay as Delay
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Bool.HT as HT
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.UUID as UUID ( toString )
import qualified Data.UUID.V1 as UUID ( nextUUID )
import qualified Database.TokyoCabinet as TC
import qualified UnexceptionalIO as UIO

import StanzaRec
import UniquePrefix
import Util
import qualified ConfigureDirectMessageRoute

sessionLifespan :: Int
sessionLifespan = 60 * 60 * seconds
	where
	seconds = 1000000

botHelp :: IQ -> Maybe Message
botHelp (IQ { iqTo = Just to, iqFrom = Just from, iqPayload = Just payload }) =
	Just $ mkSMS from to $ (s"Help:\n\t") ++ intercalate (s"\n\t") (map (\item ->
		fromMaybe mempty (attributeText (s"node") item) ++ s": " ++
		fromMaybe mempty (attributeText (s"name") item)
	) items)
	where
	items = isNamed (s"{http://jabber.org/protocol/disco#items}item") =<< elementChildren payload
botHelp _ = Nothing

commandList :: JID -> Maybe Text -> JID -> JID -> [Element] -> IQ
commandList componentJid qid from to extras =
	(emptyIQ IQResult) {
		iqTo = Just to,
		iqFrom = Just from,
		iqID = qid,
		iqPayload = Just $ Element (s"{http://jabber.org/protocol/disco#items}query")
			[(s"{http://jabber.org/protocol/disco#items}node", [ContentText $ s"http://jabber.org/protocol/commands"])]
			([
				NodeElement $ Element (s"{http://jabber.org/protocol/disco#items}item") [
						(s"jid", [ContentText $ formatJID componentJid ++ s"/CHEOGRAM%" ++ ConfigureDirectMessageRoute.nodeName]),
						(s"node", [ContentText $ ConfigureDirectMessageRoute.nodeName]),
						(s"name", [ContentText $ s"Configure direct message route"])
				] []
			] ++ extraItems)
	}
	where
	extraItems = map (\el ->
			NodeElement $ el {
				elementAttributes = map (\(aname, acontent) ->
					if aname == s"{http://jabber.org/protocol/disco#items}jid" || aname == s"jid" then
						(aname, [ContentText $ formatJID componentJid])
					else
						(aname, acontent)
				) (elementAttributes el)
			}
		) extras

withCancel :: (UIO.Unexceptional m) => Int -> (Text -> m ()) -> m () -> STM XMPP.Message -> m XMPP.Message
withCancel sessionLength sendText cancelSession getMessage = do
	delay <- fromIO_ $ Delay.newDelay sessionLength
	maybeMsg <- atomicUIO $
		(Delay.waitDelay delay *> pure Nothing)
		<|>
		Just <$> getMessage
	case maybeMsg of
		Just msg
			| getBody (s"jabber:component:accept") msg == Just (s"cancel") -> cancel $ s"cancelled"
		Just msg -> return msg
		Nothing -> cancel $ s"expired"
	where
	cancel t = do
		sendText t
		cancelSession
		fromIO_ $ myThreadId >>= killThread
		return $ error "Unreachable"

queryCommandList' :: JID -> JID -> IQ
queryCommandList' to from =
	(emptyIQ IQGet) {
		iqTo = Just to,
		iqFrom = Just from,
		iqPayload = Just $ Element (fromString "{http://jabber.org/protocol/disco#items}query") [
			(s"{http://jabber.org/protocol/disco#items}node", [ContentText $ s"http://jabber.org/protocol/commands"])
		] []
	}

queryCommandList :: JID -> JID -> IO [StanzaRec]
queryCommandList to from = do
	uuid <- (fmap.fmap) (fromString . UUID.toString) UUID.nextUUID
	return [mkStanzaRec $ (queryCommandList' to from) {iqID = uuid}]


untilParse :: (UIO.Unexceptional m) => m Message -> m () -> Atto.Parser b -> m b
untilParse getText onFail parser = do
	text <- (fromMaybe mempty . getBody "jabber:component:accept") <$> getText
	case Atto.parseOnly parser text of
		Right v -> return v
		Left _ -> do
			onFail
			untilParse getText onFail parser

adhocBotAnswerTextSingle :: (UIO.Unexceptional m) => (Text -> m ()) -> m XMPP.Message -> Element -> m [Element]
adhocBotAnswerTextSingle sendText getMessage field = do
	case attributeText (s"var") field of
		Just var -> do
			let lbl = fromMaybe (s"Enter text") $ label field
			let descSuffix = maybe mempty (\dsc -> s"\n(" ++ dsc ++ s")") $
				desc field
			let valueSuffix = maybe mempty (\val -> s" [" ++ val ++ s"] ") $
				mfilter (not . T.null) $ Just (fieldValue field)
			sendText $ lbl ++ valueSuffix ++ s":" ++ descSuffix
			value <- getMessage
			case getBody "jabber:component:accept" value of
				Just body -> return [Element (s"{jabber:x:data}field") [(s"var", [ContentText var])] [
						NodeElement $ Element (s"{jabber:x:data}value") [] [NodeContent $ ContentText body]
					]]
				Nothing -> return []
		_ -> log "ADHOC BOT FIELD WITHOUT VAR" field >> return []

adhocBotAnswerListMulti :: (UIO.Unexceptional m) => (Text -> m ()) -> m XMPP.Message -> Element -> m [Element]
adhocBotAnswerListMulti sendText getMessage field = do
	case attributeText (s"var") field of
		Just var -> do
			let label = fromMaybe (s"Select") $ attributeText (s"label") field
			let options = zip [1..] $ isNamed(s"{jabber:x:data}option") =<< elementChildren field
			let optionsText = fmap (\(n, v) -> tshow n <> s". " <> optionText v) options
			sendText $ unlines $ [label <> s": (enter numbers with commas or spaces between them)"] <> optionsText
			values <- untilParse getMessage (sendText helperText) parser
			let selectedOptions = fmap snd $ filter (\(x, _) -> x `elem` values) options
			return [Element (s"{jabber:x:data}field") [(s"var", [ContentText var])] $ flip fmap selectedOptions $ \option ->
						NodeElement $ Element (s"{jabber:x:data}value") [] [NodeContent $ ContentText $ fieldValue option]
				]
		_ -> log "ADHOC BOT FIELD WITHOUT VAR" field >> return []
	where
	parser = Atto.skipMany Atto.space *> Atto.sepBy (Atto.decimal :: Atto.Parser Int) (Atto.skipMany $ Atto.choice [Atto.space, Atto.char ',']) <* Atto.skipMany Atto.space <* Atto.endOfInput
	helperText = s"I didn't understand your answer. Please send the numbers you want, separated by commas or spaces like \"1, 3\" or \"1 3\". Blank (or just spaces) to pick nothing."

adhocBotAnswerListSingle :: (UIO.Unexceptional m) => (Text -> m ()) -> m XMPP.Message -> Element -> m [Element]
adhocBotAnswerListSingle sendText getMessage field = do
	case attributeText (s"var") field of
		Just var -> do
			let label = fromMaybe (s"Select") $ attributeText (s"label") field
			let options = zip [1..] $ isNamed(s"{jabber:x:data}option") =<< elementChildren field
			let optionsText = fmap (\(n, v) -> tshow n <> s". " <> optionText v) options
			sendText $ unlines $ [label <> s": (enter one number)"] <> optionsText
			value <- untilParse getMessage (sendText helperText) (Atto.skipMany Atto.space *> (Atto.decimal :: Atto.Parser Int) <* Atto.skipMany Atto.space)
			let maybeOption = fmap snd $ find (\(x, _) -> x == value) options
			case maybeOption of
				Just option -> return [Element (s"{jabber:x:data}field") [(s"var", [ContentText var])] [
						NodeElement $ Element (s"{jabber:x:data}value") [] [NodeContent $ ContentText $ fieldValue option]
					]]
				Nothing -> do
					sendText $ s"Please pick one of the given options"
					adhocBotAnswerListSingle sendText getMessage field
		_ -> log "ADHOC BOT FIELD WITHOUT VAR" field >> return []
	where
	helperText = s"I didn't understand your answer. Please just send the number of the one item you want to pick, like \"1\""

adhocBotAnswerForm :: (UIO.Unexceptional m) => (Text -> m ()) -> m XMPP.Message -> Element -> m Element
adhocBotAnswerForm sendText getMessage form = do
	fields <- forM (elementChildren form) $ \field -> do
		flip HT.select [
			( elementName field == s"{jabber:x:data}instructions",
				sendText (mconcat $ elementText field) >> return []),
			( elementName field == s"{jabber:x:data}field" &&
			  attributeText (s"type") field == Just (s"list-single"),
				adhocBotAnswerListSingle sendText getMessage field),
			( elementName field == s"{jabber:x:data}field" &&
			  attributeText (s"type") field == Just (s"list-multi"),
				adhocBotAnswerListMulti sendText getMessage field),
			( elementName field == s"{jabber:x:data}field" &&
			  attributeText (s"type") field `elem` [Just (s"text-single"), Nothing],
				-- The default if a type isn't specified is text-single
				adhocBotAnswerTextSingle sendText getMessage field),
			( elementName field == s"{jabber:x:data}field",
				-- The spec says a field type we don't understand should be treated as text-single
				log "ADHOC BOT UNKNOWN FIELD" field >>
				adhocBotAnswerTextSingle sendText getMessage field
			)]
			-- There can be other things in here that aren't fields, and we want to ignore them completely
			(return [])
	return $ Element (s"{jabber:x:data}x") [(s"type", [ContentText $ s"submit"])] $ NodeElement <$> mconcat fields

label :: Element -> Maybe Text
label = attributeText (s"label")

optionText :: Element -> Text
optionText element = fromMaybe (fieldValue element) (label element)

fieldValue :: Element -> Text
fieldValue = mconcat . (elementText <=< isNamed(s"{jabber:x:data}value") <=< elementChildren)

desc :: Element -> Maybe Text
desc = mfilter (not . T.null) . Just . mconcat .
	(elementText <=< isNamed(s"{jabber:x:data}desc") <=< elementChildren)

sendHelp :: (UIO.Unexceptional m, TC.TCDB db) =>
	   db
	-> JID
	-> (XMPP.Message -> STM ())
	-> (XMPP.IQ -> UIO.UIO (STM (Maybe XMPP.IQ)))
	-> JID
	-> JID
	-> m ()
sendHelp db componentJid sendMessage sendIQ from routeFrom = do
	maybeRoute <- fmap (join . hush) $ UIO.fromIO $ TC.runTCM $ TC.get db (T.unpack (bareTxt from) ++ "\0direct-message-route")
	case parseJID =<< fmap fromString maybeRoute of
		Just route -> do
			mreply <- atomicUIO =<< (UIO.lift . sendIQ) (queryCommandList' route routeFrom)
			let helpMessage = botHelp $ commandList componentJid Nothing componentJid from $
				isNamed (s"{http://jabber.org/protocol/disco#items}item") =<< elementChildren =<< maybeToList (XMPP.iqPayload =<< mfilter ((== XMPP.IQResult) . XMPP.iqType) mreply)
			case helpMessage of
				Just msg -> atomicUIO $ sendMessage msg
				Nothing -> log "INVALID HELP MESSAGE" mreply
		Nothing ->
			case botHelp $ commandList componentJid Nothing componentJid from [] of
				Just msg -> atomicUIO $ sendMessage msg
				Nothing -> log "INVALID HELP MESSAGE" ()

adhocBotRunCommand :: (TC.TCDB db, UIO.Unexceptional m) => db -> JID -> JID -> (XMPP.Message -> STM ()) -> (XMPP.IQ -> UIO.UIO (STM (Maybe XMPP.IQ))) -> STM XMPP.Message -> JID -> Text -> [Element] -> m ()
adhocBotRunCommand db componentJid routeFrom sendMessage sendIQ getMessage from body cmdEls = do
	let (nodes, cmds) = unzip $ mapMaybe (\el -> (,) <$> attributeText (s"node") el <*> pure el) cmdEls
	case snd <$> find (\(prefixes, _) -> Set.member body prefixes) (zip (uniquePrefix nodes) cmds) of
		Just cmd -> do
			let cmdIQ = (emptyIQ IQSet) {
				iqFrom = Just routeFrom,
				iqTo = parseJID =<< attributeText (s"jid") cmd,
				iqPayload = Just $ Element (s"{http://jabber.org/protocol/commands}command") [(s"node", [ContentText $ fromMaybe mempty $ attributeText (s"node") cmd])] []
			}
			sendAndRespondTo cmdIQ
		Nothing -> sendHelp db componentJid sendMessage sendIQ from routeFrom
	where
	sendAndRespondTo cmdIQ = do
		mcmdResult <- atomicUIO =<< UIO.lift (sendIQ cmdIQ)
		case mcmdResult of
			Just resultIQ
				| IQResult == iqType resultIQ,
				  Just payload <- iqPayload resultIQ,
				  [note] <- isNamed (s"{http://jabber.org/protocol/commands}note") =<< elementChildren payload ->
					atomicUIO $ sendMessage $ mkSMS componentJid from $ mconcat $ elementText note
				| IQResult == iqType resultIQ,
				  Just payload <- iqPayload resultIQ,
				  Just sessionid <- attributeText (s"sessionid") payload,
				  Just cmd <- attributeText (s"node") payload,
				  [form] <- isNamed (s"{jabber:x:data}x") =<< elementChildren payload -> do
					let threadedMessage msg = msg { messagePayloads = (Element (s"thread") [] [NodeContent $ ContentText sessionid]) : messagePayloads msg }
					let cancelIQ = (emptyIQ IQSet) {
						iqFrom = Just routeFrom,
						iqTo = iqFrom resultIQ,
						iqPayload = Just $ Element (s"{http://jabber.org/protocol/commands}command") [(s"node", [ContentText cmd]), (s"sessionid", [ContentText sessionid]), (s"action", [ContentText $ s"cancel"])] []
					}
					let cancel = void . atomicUIO =<< UIO.lift (sendIQ cancelIQ)
					let sendText = atomicUIO . sendMessage . threadedMessage . mkSMS componentJid from
					let cancelText = sendText . ((cmd <> s" ") <>)
					returnForm <- adhocBotAnswerForm sendText (withCancel sessionLifespan cancelText cancel getMessage) form
					let actions = listToMaybe $ isNamed(s"{http://jabber.org/protocol/commands}actions") =<< elementChildren payload
					-- The standard says if actions is present, with no "execute" attribute, that the default is "next"
					-- But if there is no actions, the default is "execute"
					let defaultAction = maybe (s"execute") (fromMaybe (s"next") . attributeText (s"execute")) actions
					let cmdIQ' = (emptyIQ IQSet) {
						iqFrom = Just routeFrom,
						iqTo = iqFrom resultIQ,
						iqPayload = Just $ Element (s"{http://jabber.org/protocol/commands}command") [(s"node", [ContentText $ fromMaybe mempty $ attributeText (s"node") payload]), (s"sessionid", [ContentText sessionid]), (s"action", [ContentText defaultAction])] [NodeElement returnForm]
					}
					sendAndRespondTo cmdIQ'
				| otherwise -> atomicUIO $ sendMessage $ mkSMS componentJid from (s"Command error")
			Nothing -> atomicUIO $ sendMessage $ mkSMS componentJid from (s"Command timed out")

adhocBotSession :: (UIO.Unexceptional m, TC.TCDB db) => db -> JID -> (XMPP.Message -> STM ()) -> (XMPP.IQ -> UIO.UIO (STM (Maybe XMPP.IQ))) -> STM XMPP.Message -> XMPP.Message-> m ()
adhocBotSession db componentJid sendMessage sendIQ getMessage message@(XMPP.Message { XMPP.messageFrom = Just from })
	| Just body <- getBody "jabber:component:accept" message = do
		maybeRoute <- fmap (join . hush) $ UIO.fromIO $ TC.runTCM $ TC.get db (T.unpack (bareTxt from) ++ "\0direct-message-route")
		case parseJID =<< fmap fromString maybeRoute of
			Just route -> do
				mreply <- atomicUIO =<< (UIO.lift . sendIQ) (queryCommandList' route routeFrom)
				case iqPayload =<< mfilter ((==IQResult) . iqType) mreply of
					Just reply -> adhocBotRunCommand db componentJid routeFrom sendMessage sendIQ getMessage from body (elementChildren reply)
					Nothing -> adhocBotRunCommand db componentJid routeFrom sendMessage sendIQ getMessage from body (elementChildren =<< maybeToList (iqPayload $ commandList componentJid Nothing componentJid from []))
			Nothing -> adhocBotRunCommand db componentJid routeFrom sendMessage sendIQ getMessage from body (elementChildren =<< maybeToList (iqPayload $ commandList componentJid Nothing componentJid from []))
	| otherwise = sendHelp db componentJid sendMessage sendIQ from routeFrom
	where
	Just routeFrom = parseJID $ escapeJid (bareTxt from) ++ s"@" ++ formatJID componentJid ++ s"/adhocbot"
adhocBotSession _ _ _ _ _ m = log "BAD ADHOC BOT MESSAGE" m
