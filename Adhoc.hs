module Adhoc(adhocBotSession, commandList, queryCommandList) where

import Prelude ()
import BasicPrelude hiding (log)
import Control.Concurrent (myThreadId, killThread)
import Control.Concurrent.STM
import Control.Error (hush, ExceptT, runExceptT, throwE)
import Data.XML.Types as XML (Element(..), Node(NodeContent, NodeElement), Content(ContentText), isNamed, elementText, elementChildren, attributeText)

import Network.Protocol.XMPP (JID(..), parseJID, formatJID, IQ(..), IQType(..), emptyIQ, Message(..))
import qualified Network.Protocol.XMPP as XMPP

import qualified Control.Concurrent.STM.Delay as Delay
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Bool.HT as HT
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.UUID as UUID ( toString, toText )
import qualified Data.UUID.V1 as UUID ( nextUUID )
import qualified Database.TokyoCabinet as TC
import qualified UnexceptionalIO.Trans ()
import qualified UnexceptionalIO as UIO

import StanzaRec
import UniquePrefix
import Util
import qualified ConfigureDirectMessageRoute

sessionLifespan :: Int
sessionLifespan = 60 * 60 * seconds
	where
	seconds = 1000000

addOriginUUID :: (UIO.Unexceptional m) => XMPP.Message -> m XMPP.Message
addOriginUUID msg = maybe msg (addTag msg) <$> fromIO_ UUID.nextUUID
	where
	addTag msg uuid = msg { messagePayloads = Element (s"{urn:xmpp:sid:0}origin-id") [(s"id", [ContentText $ UUID.toText uuid])] [] : messagePayloads msg }

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

withNext :: (UIO.Unexceptional m) =>
	   m XMPP.Message
	-> Element
	-> (ExceptT [Element] m XMPP.Message -> ExceptT [Element] m [Element])
	-> m [Element]
withNext getMessage field answerField
	| isRequired field && T.null (fieldValue field) = do
		either return return =<< runExceptT (answerField $ lift getMessage)
	| otherwise =
		either return return =<< runExceptT (answerField suspension)
	where
	suspension = do
		m <- lift getMessage
		if getBody (s"jabber:component:accept") m == Just (s"next") then
			throwE [field]
		else
			return m

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


untilParse :: (UIO.Unexceptional m) => m Message -> m () -> (Text -> Maybe b) -> m b
untilParse getText onFail parser = do
	text <- (fromMaybe mempty . getBody "jabber:component:accept") <$> getText
	maybe parseFail return $ parser text
	where
	parseFail = do
		onFail
		untilParse getText onFail parser

formatLabel :: (Text -> Maybe Text) -> Element -> Text
formatLabel valueFormatter field = lbl ++ value ++ descSuffix
	where
	lbl = maybe mempty T.toTitle $ label field
	value = maybe mempty (\v -> s" [Current value " ++ v ++ s"]") $ valueFormatter <=< mfilter (not . T.null) $ Just $ fieldValue field
	descSuffix = maybe mempty (\dsc -> s"\n(" ++ dsc ++ s")") $ desc field

adhocBotAnswerFixed :: (UIO.Unexceptional m) => (Text -> m ()) -> m XMPP.Message -> Element -> m [Element]
adhocBotAnswerFixed sendText _getMessage field = do
	let values = fmap (mconcat . elementText) $ isNamed (s"{jabber:x:data}value") =<< elementChildren field
	sendText $ unlines values
	return []

adhocBotAnswerBoolean :: (UIO.Unexceptional m) => (Text -> m ()) -> m XMPP.Message -> Element -> m [Element]
adhocBotAnswerBoolean sendText getMessage field = do
	case attributeText (s"var") field of
		Just var -> do
			sendText $ formatLabel (fmap formatBool . hush . Atto.parseOnly parser) field ++ s"\nYes or No?"
			value <- untilParse getMessage (sendText helperText) $ hush . Atto.parseOnly parser
			return [Element (s"{jabber:x:data}field") [(s"var", [ContentText var])] [
				NodeElement $ Element (s"{jabber:x:data}value") [] [NodeContent $ ContentText $ HT.if' value (s"true") (s"false")]
				]]
		_ -> log "ADHOC BOT FIELD WITHOUT VAR" field >> return []
	where
	helperText = s"I didn't understand your answer. Please send yes or no"
	parser = Atto.skipMany Atto.space *> (
		(True <$ Atto.choice (Atto.asciiCI <$> [s"true", s"t", s"1", s"yes", s"y", s"enable", s"enabled"])) <|>
		(False <$ Atto.choice (Atto.asciiCI <$> [s"false", s"f", s"0", s"no", s"n", s"disable", s"disabled"]))
		) <* Atto.skipMany Atto.space <* Atto.endOfInput
	formatBool True = s"Yes"
	formatBool False = s"No"

adhocBotAnswerTextSingle :: (UIO.Unexceptional m) => (Text -> m ()) -> m XMPP.Message -> Element -> m [Element]
adhocBotAnswerTextSingle sendText getMessage field = do
	case attributeText (s"var") field of
		Just var -> do
			sendText $ s"Enter " ++ formatLabel Just field
			value <- getMessage
			case getBody "jabber:component:accept" value of
				Just body -> return [Element (s"{jabber:x:data}field") [(s"var", [ContentText var])] [
						NodeElement $ Element (s"{jabber:x:data}value") [] [NodeContent $ ContentText body]
					]]
				Nothing -> return []
		_ -> log "ADHOC BOT FIELD WITHOUT VAR" field >> return []

listOptionText :: (Foldable t) => t Text -> Text -> (Int, Element) -> Text
listOptionText currentValues currentValueText (n, v) = tshow n ++ s". " ++ optionText v ++ selectedText v
	where
	selectedText option
		| fieldValue option `elem` currentValues = currentValueText
		| otherwise = mempty

adhocBotAnswerJidSingle :: (UIO.Unexceptional m) => (Text -> m ()) -> m XMPP.Message -> Element -> m [Element]
adhocBotAnswerJidSingle sendText getMessage field = do
	case attributeText (s"var") field of
		Just var -> do
			sendText $ s"Enter " ++ formatLabel Just field
			value <- untilParse getMessage (sendText helperText) XMPP.parseJID
			return [Element (s"{jabber:x:data}field") [(s"var", [ContentText var])] [
				NodeElement $ Element (s"{jabber:x:data}value") [] [NodeContent $ ContentText $ formatJID value]
				]]
		_ -> log "ADHOC BOT FIELD WITHOUT VAR" field >> return []
	where
	helperText = s"I didn't understand your answer. Please send only a valid JID like person@example.com or perhaps just example.com"

adhocBotAnswerListMulti :: (UIO.Unexceptional m) => (Text -> m ()) -> m XMPP.Message -> Element -> m [Element]
adhocBotAnswerListMulti sendText getMessage field = do
	case attributeText (s"var") field of
		Just var -> do
			let options = zip [1..] $ isNamed(s"{jabber:x:data}option") =<< elementChildren field
			let currentValues = elementText =<< isNamed(s"{jabber:x:data}value") =<< elementChildren field
			let optionsText = fmap (listOptionText currentValues (s" [Currently Selected]")) options
			sendText $ unlines $ [formatLabel (const Nothing) field] ++ optionsText ++ [s"Which numbers?"]
			values <- untilParse getMessage (sendText helperText) (hush . Atto.parseOnly parser)
			let selectedOptions = fmap snd $ filter (\(x, _) -> x `elem` values) options
			return [Element (s"{jabber:x:data}field") [(s"var", [ContentText var])] $ flip fmap selectedOptions $ \option ->
						NodeElement $ Element (s"{jabber:x:data}value") [] [NodeContent $ ContentText $ fieldValue option]
				]
		_ -> log "ADHOC BOT FIELD WITHOUT VAR" field >> return []
	where
	parser = Atto.skipMany Atto.space *> Atto.sepBy Atto.decimal (Atto.skipMany $ Atto.choice [Atto.space, Atto.char ',']) <* Atto.skipMany Atto.space <* Atto.endOfInput
	helperText = s"I didn't understand your answer. Please send the numbers you want, separated by commas or spaces like \"1, 3\" or \"1 3\". Blank (or just spaces) to pick nothing."

adhocBotAnswerListSingle :: (UIO.Unexceptional m) => (Text -> m ()) -> m XMPP.Message -> Element -> m [Element]
adhocBotAnswerListSingle sendText getMessage field = do
	case attributeText (s"var") field of
		Just var -> do
			let options = zip [1..] $ isNamed(s"{jabber:x:data}option") =<< elementChildren field
			let currentValue = listToMaybe $ elementText =<< isNamed(s"{jabber:x:data}value") =<< elementChildren field
			let optionsText = fmap (listOptionText currentValue (s" [Current Value]")) options
			sendText $ unlines $ [formatLabel (const Nothing) field] ++ optionsText ++ [s"Which number?"]
			value <- untilParse getMessage (sendText helperText) (hush . Atto.parseOnly parser)
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
	parser = Atto.skipMany Atto.space *> Atto.decimal <* Atto.skipMany Atto.space

adhocBotAnswerForm :: (UIO.Unexceptional m) => (Text -> m ()) -> m XMPP.Message -> Element -> m Element
adhocBotAnswerForm sendText getMessage form = do
	fields <- forM (filter (uncurry (||) . (isField &&& isInstructions)) $ elementChildren form) $ \field ->
		let sendText' = lift . sendText in
		withNext getMessage field $ \getMessage' ->
		HT.select (
			-- The spec says a field type we don't understand should be treated as text-single
			log "ADHOC BOT UNKNOWN FIELD" field >>
			adhocBotAnswerTextSingle sendText' getMessage' field
		) [
			(isInstructions field,
				sendText' (mconcat $ elementText field) >> return []),
			(attributeText (s"type") field == Just (s"list-single"),
				adhocBotAnswerListSingle sendText' getMessage' field),
			(attributeText (s"type") field == Just (s"list-multi"),
				adhocBotAnswerListMulti sendText' getMessage' field),
			(attributeText (s"type") field == Just (s"jid-single"),
				adhocBotAnswerJidSingle sendText' getMessage' field),
			(attributeText (s"type") field == Just (s"hidden"),
				return [field]),
			(attributeText (s"type") field == Just (s"fixed"),
				adhocBotAnswerFixed sendText' getMessage' field),
			(attributeText (s"type") field == Just (s"boolean"),
				adhocBotAnswerBoolean sendText' getMessage' field),
			(attributeText (s"type") field `elem` [Just (s"text-single"), Nothing],
				-- The default if a type isn't specified is text-single
				adhocBotAnswerTextSingle sendText' getMessage' field)
		]
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

isField :: Element -> Bool
isField el = elementName el == s"{jabber:x:data}field"

isInstructions :: Element -> Bool
isInstructions el = elementName el == s"{jabber:x:data}instructions"

isRequired :: Element -> Bool
isRequired = not . null . (isNamed (s"{jabber:x:data}required") <=< elementChildren)

sendHelp :: (UIO.Unexceptional m, TC.TCDB db) =>
	   db
	-> JID
	-> (XMPP.Message -> m ())
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
				Just msg -> sendMessage msg
				Nothing -> log "INVALID HELP MESSAGE" mreply
		Nothing ->
			case botHelp $ commandList componentJid Nothing componentJid from [] of
				Just msg -> sendMessage msg
				Nothing -> log "INVALID HELP MESSAGE" ()

adhocBotRunCommand :: (TC.TCDB db, UIO.Unexceptional m) => db -> JID -> JID -> (XMPP.Message -> m ()) -> (XMPP.IQ -> UIO.UIO (STM (Maybe XMPP.IQ))) -> STM XMPP.Message -> JID -> Text -> [Element] -> m ()
adhocBotRunCommand db componentJid routeFrom sendMessage sendIQ getMessage from body cmdEls = do
	let (nodes, cmds) = unzip $ mapMaybe (\el -> (,) <$> attributeText (s"node") el <*> pure el) cmdEls
	case snd <$> find (\(prefixes, _) -> Set.member body prefixes) (zip (uniquePrefix nodes) cmds) of
		Just cmd -> do
			let cmdIQ = (emptyIQ IQSet) {
				iqFrom = Just routeFrom,
				iqTo = parseJID =<< attributeText (s"jid") cmd,
				iqPayload = Just $ Element (s"{http://jabber.org/protocol/commands}command") [(s"node", [ContentText $ fromMaybe mempty $ attributeText (s"node") cmd])] []
			}
			sendAndRespondTo (Just $ intercalate (s"\n") [
					s"You can leave something at the current value by saying 'next'.",
					s"You can return to the main menu by saying 'cancel' at any time."
				]) cmdIQ
		Nothing -> sendHelp db componentJid sendMessage sendIQ from routeFrom
	where
	sendAndRespondTo intro cmdIQ = do
		mcmdResult <- atomicUIO =<< UIO.lift (sendIQ cmdIQ)
		case mcmdResult of
			Just resultIQ
				| IQResult == iqType resultIQ,
				  Just payload <- iqPayload resultIQ,
				  [note] <- isNamed (s"{http://jabber.org/protocol/commands}note") =<< elementChildren payload ->
					sendMessage $ mkSMS componentJid from $ mconcat $ elementText note
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
					let sendText = sendMessage . threadedMessage . mkSMS componentJid from
					let cancelText = sendText . ((cmd ++ s" ") ++)
					forM_ intro sendText
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
					sendAndRespondTo Nothing cmdIQ'
				| otherwise -> sendMessage $ mkSMS componentJid from (s"Command error")
			Nothing -> sendMessage $ mkSMS componentJid from (s"Command timed out")

adhocBotSession :: (UIO.Unexceptional m, TC.TCDB db) => db -> JID -> (XMPP.Message -> m ()) -> (XMPP.IQ -> UIO.UIO (STM (Maybe XMPP.IQ))) -> STM XMPP.Message -> XMPP.Message-> m ()
adhocBotSession db componentJid sendMessage sendIQ getMessage message@(XMPP.Message { XMPP.messageFrom = Just from })
	| Just body <- getBody "jabber:component:accept" message = do
		maybeRoute <- fmap (join . hush) $ UIO.fromIO $ TC.runTCM $ TC.get db (T.unpack (bareTxt from) ++ "\0direct-message-route")
		case parseJID =<< fmap fromString maybeRoute of
			Just route -> do
				mreply <- atomicUIO =<< (UIO.lift . sendIQ) (queryCommandList' route routeFrom)
				case iqPayload =<< mfilter ((==IQResult) . iqType) mreply of
					Just reply -> adhocBotRunCommand db componentJid routeFrom sendMessage' sendIQ getMessage from body $ elementChildren reply ++ internalCommands
					Nothing -> adhocBotRunCommand db componentJid routeFrom sendMessage' sendIQ getMessage from body internalCommands
			Nothing -> adhocBotRunCommand db componentJid routeFrom sendMessage' sendIQ getMessage from body internalCommands
	| otherwise = sendHelp db componentJid sendMessage' sendIQ from routeFrom
	where
	internalCommands = elementChildren =<< maybeToList (iqPayload $ commandList componentJid Nothing componentJid from [])
	Just routeFrom = parseJID $ escapeJid (bareTxt from) ++ s"@" ++ formatJID componentJid ++ s"/adhocbot"
	sendMessage' = sendMessage <=< addOriginUUID
adhocBotSession _ _ _ _ _ m = log "BAD ADHOC BOT MESSAGE" m
