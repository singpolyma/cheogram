module Adhoc(adhocBotSession, commandList, queryCommandList) where

import Prelude ()
import BasicPrelude hiding (log)
import Control.Concurrent.STM
import Control.Error (hush)
import Data.XML.Types as XML (Element(..), Node(NodeContent, NodeElement), Content(ContentText), isNamed, elementText, elementChildren, attributeText)

import Network.Protocol.XMPP (JID(..), parseJID, formatJID, IQ(..), IQType(..), emptyIQ, Message(..))
import qualified Network.Protocol.XMPP as XMPP

import qualified Data.Attoparsec.Text as Atto
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


untilParse :: (UIO.Unexceptional m) => STM Message -> STM () -> Atto.Parser b -> m b
untilParse getText onFail parser = do
	text <- atomicUIO $ (fromMaybe mempty . getBody "jabber:component:accept") <$> getText
	case Atto.parseOnly parser text of
		Right v -> return v
		Left _ -> do
			atomicUIO onFail
			untilParse getText onFail parser

adhocBotAnswerTextSingle :: (UIO.Unexceptional m) => JID -> (XMPP.Message -> STM ()) -> STM XMPP.Message -> JID -> Element -> m [Element]
adhocBotAnswerTextSingle componentJid sendMessage getMessage from field = do
	case attributeText (s"var") field of
		Just var -> do
			let label = fromMaybe (s"Input") $ attributeText (s"label") field
			atomicUIO $ sendMessage $ mkSMS componentJid from label
			value <- atomicUIO getMessage
			case getBody "jabber:component:accept" value of
				Just body -> return [Element (s"{jabber:x:data}field") [(s"var", [ContentText var])] [
						NodeElement $ Element (s"{jabber:x:data}value") [] [NodeContent $ ContentText body]
					]]
				Nothing -> return []
		_ -> log "ADHOC BOT FIELD WITHOUT VAR" field >> return []

adhocBotAnswerListMulti :: (UIO.Unexceptional m) => JID -> (XMPP.Message -> STM ()) -> STM XMPP.Message -> JID -> Element -> m [Element]
adhocBotAnswerListMulti componentJid sendMessage getMessage from field = do
	case attributeText (s"var") field of
		Just var -> do
			let label = fromMaybe (s"Select") $ attributeText (s"label") field
			let options = zip [1..] $ isNamed(s"{jabber:x:data}option") =<< elementChildren field
			let optionsText = fmap (\(n, v) -> tshow n <> s". " <> optionText v) options
			atomicUIO $ sendMessage $ mkSMS componentJid from $ unlines $ [label <> s": (enter numbers with commas or spaces between them)"] <> optionsText
			values <- untilParse getMessage (sendMessage $ mkSMS componentJid from helperText) parser
			let selectedOptions = fmap snd $ filter (\(x, _) -> x `elem` values) options
			return [Element (s"{jabber:x:data}field") [(s"var", [ContentText var])] $ flip fmap selectedOptions $ \option ->
						NodeElement $ Element (s"{jabber:x:data}value") [] [NodeContent $ ContentText $ optionValue option]
				]
		_ -> log "ADHOC BOT FIELD WITHOUT VAR" field >> return []
	where
	parser = Atto.skipMany Atto.space *> Atto.sepBy (Atto.decimal :: Atto.Parser Int) (Atto.skipMany $ Atto.choice [Atto.space, Atto.char ',']) <* Atto.skipMany Atto.space <* Atto.endOfInput
	helperText = s"I didn't understand your answer. Please send the numbers you want, separated by commas or spaces like \"1, 3\" or \"1 3\". Blank (or just spaces) to pick nothing."

adhocBotAnswerListSingle :: (UIO.Unexceptional m) => JID -> (XMPP.Message -> STM ()) -> STM XMPP.Message -> JID -> Element -> m [Element]
adhocBotAnswerListSingle componentJid sendMessage getMessage from field = do
	case attributeText (s"var") field of
		Just var -> do
			let label = fromMaybe (s"Select") $ attributeText (s"label") field
			let options = zip [1..] $ isNamed(s"{jabber:x:data}option") =<< elementChildren field
			let optionsText = fmap (\(n, v) -> tshow n <> s". " <> optionText v) options
			atomicUIO $ sendMessage $ mkSMS componentJid from $ unlines $ [label <> s": (enter one number)"] <> optionsText
			value <- untilParse getMessage (sendMessage $ mkSMS componentJid from helperText) (Atto.skipMany Atto.space *> (Atto.decimal :: Atto.Parser Int) <* Atto.skipMany Atto.space)
			let maybeOption = fmap snd $ find (\(x, _) -> x == value) options
			case maybeOption of
				Just option -> return [Element (s"{jabber:x:data}field") [(s"var", [ContentText var])] [
						NodeElement $ Element (s"{jabber:x:data}value") [] [NodeContent $ ContentText $ optionValue option]
					]]
				Nothing -> do
					atomicUIO $ sendMessage $ mkSMS componentJid from $ s"Please pick one of the given options"
					adhocBotAnswerListSingle componentJid sendMessage getMessage from field
		_ -> log "ADHOC BOT FIELD WITHOUT VAR" field >> return []
	where
	helperText = s"I didn't understand your answer. Please just send the number of the one item you want to pick, like \"1\""

adhocBotAnswerForm :: (UIO.Unexceptional m) => JID -> (XMPP.Message -> STM ()) -> STM XMPP.Message -> JID -> Element -> m Element
adhocBotAnswerForm componentJid sendMessage getMessage from form = do
	fields <- forM (elementChildren form) $ \field -> do
		case field of
			_
				| elementName field == s"{jabber:x:data}instructions" -> atomicUIO (sendMessage $ mkSMS componentJid from $ mconcat $ elementText field) >> return []
			_
				| elementName field == s"{jabber:x:data}field",
				  attributeText (s"type") field == Just (s"list-single") ->
					adhocBotAnswerListSingle componentJid sendMessage getMessage from field
			_
				| elementName field == s"{jabber:x:data}field",
				  attributeText (s"type") field == Just (s"list-multi") ->
					adhocBotAnswerListMulti componentJid sendMessage getMessage from field
			_
				| elementName field == s"{jabber:x:data}field",
				  attributeText (s"type") field `elem` [Just (s"text-single"), Nothing] ->
					-- The default if a type isn't specified is text-single
					adhocBotAnswerTextSingle componentJid sendMessage getMessage from field
			_
				| elementName field == s"{jabber:x:data}field" -> do
					-- The spec says a field type we don't understand should be treated as text-single
					log "ADHOC BOT UNKNOWN FIELD" field
					adhocBotAnswerTextSingle componentJid sendMessage getMessage from field
			-- There can be other things in here that aren't fields, and we want to ignore them completely
			_ -> return []
	return $ Element (s"{jabber:x:data}x") [(s"type", [ContentText $ s"submit"])] $ NodeElement <$> mconcat fields

optionText :: Element -> Text
optionText element = fromMaybe (optionValue element) $ attributeText (s"label") element

optionValue :: Element -> Text
optionValue element = mconcat $ elementText =<< isNamed(s"{jabber:x:data}value") =<< elementChildren element

adhocBotRunCommand :: (UIO.Unexceptional m) => JID -> JID -> (XMPP.Message -> STM ()) -> (XMPP.IQ -> UIO.UIO (STM (Maybe XMPP.IQ))) -> STM XMPP.Message -> JID -> Text -> [Element] -> m ()
adhocBotRunCommand componentJid routeFrom sendMessage sendIQ getMessage from body cmdEls = do
	let (nodes, cmds) = unzip $ mapMaybe (\el -> (,) <$> attributeText (s"node") el <*> pure el) cmdEls
	case snd <$> find (\(prefixes, _) -> Set.member body prefixes) (zip (uniquePrefix nodes) cmds) of
		Just cmd -> do
			let cmdIQ = (emptyIQ IQSet) {
				iqFrom = Just routeFrom,
				iqTo = parseJID =<< attributeText (s"jid") cmd,
				iqPayload = Just $ Element (s"{http://jabber.org/protocol/commands}command") [(s"node", [ContentText $ fromMaybe mempty $ attributeText (s"node") cmd])] []
			}
			sendAndRespondTo cmdIQ
		Nothing -> atomicUIO $ sendMessage $ mkSMS componentJid from (s"Instead of sending messages to " ++ formatJID componentJid ++ s" directly, you can SMS your contacts by sending messages to +1<phone-number>@" ++ formatJID componentJid ++ s" Jabber IDs.  Or, for support, come talk to us in xmpp:discuss@conference.soprani.ca?join")
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
				  [form] <- isNamed (s"{jabber:x:data}x") =<< elementChildren payload -> do
					let threadedMessage msg = msg { messagePayloads = (Element (s"thread") [] [NodeContent $ ContentText sessionid]) : messagePayloads msg }
					returnForm <- adhocBotAnswerForm componentJid (sendMessage . threadedMessage) getMessage from form
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
	| Just body <- getBody "jabber:component:accept" message,
	  body == s"help" = do
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
	| Just body <- getBody "jabber:component:accept" message = do
		maybeRoute <- fmap (join . hush) $ UIO.fromIO $ TC.runTCM $ TC.get db (T.unpack (bareTxt from) ++ "\0direct-message-route")
		case parseJID =<< fmap fromString maybeRoute of
			Just route -> do
				mreply <- atomicUIO =<< (UIO.lift . sendIQ) (queryCommandList' route routeFrom)
				case iqPayload =<< mfilter ((==IQResult) . iqType) mreply of
					Just reply -> adhocBotRunCommand componentJid routeFrom sendMessage sendIQ getMessage from body (elementChildren reply)
					Nothing -> adhocBotRunCommand componentJid routeFrom sendMessage sendIQ getMessage from body (elementChildren =<< maybeToList (iqPayload $ commandList componentJid Nothing componentJid from []))
			Nothing -> adhocBotRunCommand componentJid routeFrom sendMessage sendIQ getMessage from body (elementChildren =<< maybeToList (iqPayload $ commandList componentJid Nothing componentJid from []))
	| otherwise =
		atomicUIO $ sendMessage $ mkSMS componentJid from (s"Instead of sending messages to " ++ formatJID componentJid ++ s" directly, you can SMS your contacts by sending messages to +1<phone-number>@" ++ formatJID componentJid ++ s" Jabber IDs.  Or, for support, come talk to us in xmpp:discuss@conference.soprani.ca?join")
	where
	Just routeFrom = parseJID $ escapeJid (bareTxt from) ++ s"@" ++ formatJID componentJid ++ s"/adhocbot"
adhocBotSession _ _ _ _ _ m = log "BAD ADHOC BOT MESSAGE" m
