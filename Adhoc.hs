module Adhoc (adhocBotSession, commandList, queryCommandList) where

import Prelude ()
import BasicPrelude hiding (log)
import Control.Concurrent (myThreadId, killThread)
import Control.Concurrent.STM
import Control.Error (hush, ExceptT, runExceptT, throwE, justZ)
import Data.XML.Types as XML (Element(..), Node(NodeContent, NodeElement), Content(ContentText), isNamed, elementText, elementChildren, attributeText)
import qualified Data.XML.Types as XML

import Network.Protocol.XMPP (JID(..), parseJID, formatJID, IQ(..), IQType(..), emptyIQ, Message(..))
import qualified Network.Protocol.XMPP as XMPP

import qualified Data.CaseInsensitive as CI
import qualified Control.Concurrent.STM.Delay as Delay
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Bool.HT as HT
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.UUID as UUID ( toString, toText )
import qualified Data.UUID.V1 as UUID ( nextUUID )
import qualified UnexceptionalIO.Trans ()
import qualified UnexceptionalIO as UIO

import StanzaRec
import UniquePrefix
import Util
import qualified ConfigureDirectMessageRoute
import qualified DB

sessionLifespan :: Int
sessionLifespan = 60 * 60 * seconds
	where
	seconds = 1000000

addOriginUUID :: (UIO.Unexceptional m) => XMPP.Message -> m XMPP.Message
addOriginUUID msg = maybe msg (addTag msg) <$> fromIO_ UUID.nextUUID
	where
	addTag msg uuid = msg { messagePayloads = Element (s"{urn:xmpp:sid:0}origin-id") [(s"id", [ContentText $ UUID.toText uuid])] [] : messagePayloads msg }

botHelp :: Maybe Text -> IQ -> Maybe Message
botHelp header (IQ { iqTo = Just to, iqFrom = Just from, iqPayload = Just payload }) =
	Just $ mkSMS from to $ maybe mempty (++ s"\n") header ++ (s"Help:\n\t") ++ intercalate (s"\n\t") (map (\item ->
		fromMaybe mempty (attributeText (s"node") item) ++ s": " ++
		fromMaybe mempty (attributeText (s"name") item)
	) items)
	where
	items = isNamed (s"{http://jabber.org/protocol/disco#items}item") =<< elementChildren payload
botHelp _ _ = Nothing

commandList :: JID -> Maybe Text -> JID -> JID -> [Element] -> IQ
commandList componentJid qid from to extras =
	(emptyIQ IQResult) {
		iqTo = Just to,
		iqFrom = Just from,
		iqID = qid,
		iqPayload = Just $ Element (s"{http://jabber.org/protocol/disco#items}query")
			[(s"{http://jabber.org/protocol/disco#items}node", [ContentText $ s"http://jabber.org/protocol/commands"])]
			(extraItems ++ [
				NodeElement $ Element (s"{http://jabber.org/protocol/disco#items}item") [
						(s"jid", [ContentText $ formatJID componentJid ++ s"/CHEOGRAM%" ++ ConfigureDirectMessageRoute.nodeName]),
						(s"node", [ContentText $ ConfigureDirectMessageRoute.nodeName]),
						(s"name", [ContentText $ s"Register with backend"])
				] []
			])
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
		) $ filter (\el ->
			attributeText (s"node") el /= Just (s"jabber:iq:register")
		) extras

withNext :: (UIO.Unexceptional m) =>
	   m XMPP.Message
	-> Element
	-> (ExceptT [Element] m XMPP.Message -> ExceptT [Element] m [Element])
	-> m [Element]
withNext getMessage field answerField
	| isRequired field && T.null (mconcat $ fieldValue field) = do
		either return return =<< runExceptT (answerField $ lift getMessage)
	| otherwise =
		either return return =<< runExceptT (answerField suspension)
	where
	suspension = do
		m <- lift getMessage
		if fmap CI.mk (getBody (s"jabber:component:accept") m) == Just (s"next") then
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
			| (CI.mk <$> getBody (s"jabber:component:accept") msg) == Just (s"cancel") -> cancel $ s"cancelled"
		Just msg -> return msg
		Nothing -> cancel $ s"expired"
	where
	cancel t = do
		sendText t
		cancelSession
		fromIO_ $ myThreadId >>= killThread
		return $ error "Unreachable"

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
	lbl = maybe (fromMaybe mempty $ attributeText (s"var") field) T.toTitle $ label field
	value = maybe mempty (\v -> s" [Current value " ++ v ++ s"]") $ valueFormatter <=< mfilter (not . T.null) $ Just $ intercalate (s", ") (fieldValue field)
	descSuffix = maybe mempty (\dsc -> s"\n(" ++ dsc ++ s")") $ desc field

adhocBotAnswerFixed :: (UIO.Unexceptional m) => (Text -> m ()) -> m XMPP.Message -> Element -> m [Element]
adhocBotAnswerFixed sendText _getMessage field = do
	let values = fmap (mconcat . elementText) $ isNamed (s"{jabber:x:data}value") =<< elementChildren field
	sendText $ formatLabel (const Nothing) field
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
		| mconcat (fieldValue option) `elem` currentValues = currentValueText
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
			let currentValues = fieldValue field
			let optionsText = fmap (listOptionText currentValues (s" [Currently Selected]")) options
			sendText $ unlines $ [formatLabel (const Nothing) field] ++ optionsText ++ [s"Which numbers?"]
			values <- untilParse getMessage (sendText helperText) (hush . Atto.parseOnly parser)
			let selectedOptions = fmap snd $ filter (\(x, _) -> x `elem` values) options
			return [Element (s"{jabber:x:data}field") [(s"var", [ContentText var])] $ flip fmap selectedOptions $ \option ->
						NodeElement $ Element (s"{jabber:x:data}value") [] [NodeContent $ ContentText $ mconcat $ fieldValue option]
				]
		_ -> log "ADHOC BOT FIELD WITHOUT VAR" field >> return []
	where
	parser = Atto.skipMany Atto.space *> Atto.sepBy Atto.decimal (Atto.skipMany $ Atto.choice [Atto.space, Atto.char ',']) <* Atto.skipMany Atto.space <* Atto.endOfInput
	helperText = s"I didn't understand your answer. Please send the numbers you want, separated by commas or spaces like \"1, 3\" or \"1 3\". Blank (or just spaces) to pick nothing."

-- Technically an option can have multiple values, but in practice it probably won't
data ListSingle = ListSingle { listLabel :: T.Text, listIsOpen :: Bool, listLookup :: (Int -> Maybe [T.Text]) }
listSingleHelper :: Element -> ListSingle
listSingleHelper field = ListSingle label open lookup
	where
	open = isOpenValidation (fieldValidation field)
	options = zip [1..] $ isNamed(s"{jabber:x:data}option") =<< elementChildren field
	currentValue = listToMaybe $ elementText =<< isNamed(s"{jabber:x:data}value") =<< elementChildren field
	optionsText = fmap (listOptionText currentValue (s" [Current Value]")) options
	currentValueText = (\func -> maybe [] func currentValue) $ \value ->
		if open && value `notElem` (map (mconcat . fieldValue . snd) options) then
			[s"[Current Value: " ++ value ++ s"]"]
		else
			[]
	label = unlines $ [formatLabel (const Nothing) field] ++ optionsText ++ currentValueText
	lookup n = fmap (fieldValue . snd) $ find (\(x, _) -> x == n) options

adhocBotAnswerListSingle :: (UIO.Unexceptional m) => (Text -> m ()) -> m XMPP.Message -> Element -> m [Element]
adhocBotAnswerListSingle sendText getMessage field = do
	case attributeText (s"var") field of
		Just var -> do
			let list = listSingleHelper field
			let open = listIsOpen list
			let prompt = s"Please enter a number from the list above" ++ if open then s", or enter a custom option" else s""
			sendText $ listLabel list ++ prompt
			maybeOption <- if open then do
					value <- untilParse getMessage (sendText helperText) (hush . Atto.parseOnly openParser)
					return $ case value of
						Left openValue -> Just [openValue]
						Right itemNumber -> listLookup list itemNumber
				else do
					value <- untilParse getMessage (sendText helperText) (hush . Atto.parseOnly parser)
					return $ listLookup list value
			case maybeOption of
				Just option -> return [Element (s"{jabber:x:data}field") [(s"var", [ContentText var])] [
						NodeElement $ Element (s"{jabber:x:data}value") [] [NodeContent $ ContentText $ mconcat option]
					]]
				Nothing -> do
					sendText $ s"Please pick one of the given options"
					adhocBotAnswerListSingle sendText getMessage field
		_ -> log "ADHOC BOT FIELD WITHOUT VAR" field >> return []
	where
	helperText = s"I didn't understand your answer. Please just send the number of the one item you want to pick, like \"1\""
	parser = Atto.skipMany Atto.space *> Atto.decimal <* Atto.skipMany Atto.space
	openParser = (Right <$> parser) <|> (Left <$> Atto.takeText)

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

formatReported :: Element -> (Text, [Text])
formatReported =
	first (intercalate (s"\t")) .  unzip .
	map (\field ->
		(
			formatLabel (const Nothing) field,
			fromMaybe mempty (attributeText (s"var") field)
		)
	) . filter isField . elementChildren

formatItem :: [Text] -> Element -> Text
formatItem reportedVars item = intercalate (s"\t") $ map (\var ->
		intercalate (s", ") $ findFieldValue var
	) reportedVars
	where
	findFieldValue var = maybe [] fieldValue $ find (\field ->
			attributeText (s"var") field == Just var
		) fields
	fields = filter isField $ elementChildren item

formatResultField :: Element -> Text
formatResultField el = case maybe ("text-single") T.unpack $ attributeText (s"type") el of
	"hidden" -> mempty
	"list-single" -> listLabel $ listSingleHelper el
	"fixed" -> concat [label, s":\n", unlines (fieldValue el)]
	"jid-single" -> concat [
		label,
		s": ",
		(unlines $ (s"xmpp:" ++) <$> (fieldValue el))
		]
	_ -> concat [label, s": ", unlines (fieldValue el)]
	where
	label = formatLabel (const Nothing) el

renderResultForm :: Element -> Text
renderResultForm form =
	intercalate (s"\n") $ catMaybes $ snd $
	forAccumL [] (elementChildren form) $ \reportedVars el ->
		HT.select (reportedVars, Nothing) $ map (second $ second Just) [
			(isInstructions el, (reportedVars,
				mconcat $ elementText el)),
			(isField el, (reportedVars, formatResultField el)),
			(isReported el,
				swap $ formatReported el),
			(isItem el, (reportedVars,
				formatItem reportedVars el))
		]
	where
	forAccumL z xs f = mapAccumL f z xs

data Action = ActionNext | ActionPrev | ActionCancel | ActionComplete

actionContent :: Action -> Content
actionContent ActionNext     = ContentText $ s"next"
actionContent ActionPrev     = ContentText $ s"prev"
actionContent ActionCancel   = ContentText $ s"cancel"
actionContent ActionComplete = ContentText $ s"complete"

actionCmd :: Action -> Text
actionCmd ActionNext     = s"next"
actionCmd ActionPrev     = s"back"
actionCmd ActionCancel   = s"cancel"
actionCmd ActionComplete = s"finish"

actionFromXMPP :: Text -> Maybe Action
actionFromXMPP xmpp
	| xmpp == s"next"     = Just ActionNext
	| xmpp == s"prev"     = Just ActionPrev
	| xmpp == s"complete" = Just ActionComplete
	| otherwise           = Nothing

waitForAction :: (UIO.Unexceptional m) => [Action] -> (Text -> m ()) -> m XMPP.Message -> m Action
waitForAction actions sendText getMessage = do
	m <- getMessage
	let ciBody = CI.mk <$> getBody (s"jabber:component:accept") m
	HT.select whatWasThat [
			(ciBody == Just (s"next"), return ActionNext),
			(ciBody == Just (s"back"), return ActionPrev),
			(ciBody == Just (s"cancel"), return ActionCancel),
			(ciBody == Just (s"finish"), return ActionComplete)
		]
	where
	allowedCmds = map actionCmd (ActionCancel : actions)
	whatWasThat = do
		sendText $
			s"I didn't understand that. You can say one of: " ++
			intercalate (s", ") allowedCmds
		waitForAction actions sendText getMessage

label :: Element -> Maybe Text
label = attributeText (s"label")

optionText :: Element -> Text
optionText element = fromMaybe (mconcat $ fieldValue element) (label element)

fieldValue :: Element -> [Text]
fieldValue = fmap (mconcat . elementText) .
	isNamed (s"{jabber:x:data}value") <=< elementChildren

fieldValidation :: Element -> Maybe Element
fieldValidation =
	listToMaybe .
	(isNamed (s"{http://jabber.org/protocol/xdata-validate}validate") <=< elementChildren)

isOpenValidation :: Maybe Element -> Bool
isOpenValidation (Just el) =
	not $ null $
	isNamed (s"{http://jabber.org/protocol/xdata-validate}open")
	=<< elementChildren el
isOpenValidation _ = False

desc :: Element -> Maybe Text
desc = mfilter (not . T.null) . Just . mconcat .
	(elementText <=< isNamed(s"{jabber:x:data}desc") <=< elementChildren)

isField :: Element -> Bool
isField el = elementName el == s"{jabber:x:data}field"

isInstructions :: Element -> Bool
isInstructions el = elementName el == s"{jabber:x:data}instructions"

isReported :: Element -> Bool
isReported el = elementName el == s"{jabber:x:data}reported"

isItem :: Element -> Bool
isItem el = elementName el == s"{jabber:x:data}item"

isRequired :: Element -> Bool
isRequired = not . null . (isNamed (s"{jabber:x:data}required") <=< elementChildren)

registerShorthand :: Text -> Maybe JID
registerShorthand body = do
	gatewayJID <- hush $ Atto.parseOnly (Atto.asciiCI (s"register") *> Atto.many1 Atto.space *> Atto.takeText) body
	parseJID gatewayJID

getServerInfoForm :: [XML.Element] -> Maybe XML.Element
getServerInfoForm = find (\el ->
		attributeText (s"type") el == Just (s"result") &&
		getFormField el (s"FORM_TYPE") == Just (s"http://jabber.org/network/serverinfo")
	) . (isNamed (s"{jabber:x:data}x") =<<)

sendHelp :: (UIO.Unexceptional m) =>
	   DB.DB
	-> JID
	-> (XMPP.Message -> m ())
	-> (XMPP.IQ -> UIO.UIO (STM (Maybe XMPP.IQ)))
	-> JID
	-> JID
	-> m ()
sendHelp db componentJid sendMessage sendIQ from routeFrom = do
	maybeRoute <- (parseJID =<<) . (join . hush) <$> UIO.fromIO (DB.get db (DB.byJid from ["direct-message-route"]))
	case maybeRoute of
		Just route -> do
			replySTM <- UIO.lift $ sendIQ $ queryCommandList' route routeFrom
			discoInfoSTM <- UIO.lift $ sendIQ $ queryDiscoWithNode' Nothing route routeFrom
			(mreply, mDiscoInfo) <- atomicUIO $ (,) <$> replySTM <*> discoInfoSTM

			let helpMessage = botHelp
				(renderResultForm <$> (getServerInfoForm . elementChildren =<< iqPayload =<< mDiscoInfo)) $
				commandList componentJid Nothing componentJid from $
					isNamed (s"{http://jabber.org/protocol/disco#items}item") =<< elementChildren =<< maybeToList (XMPP.iqPayload =<< mfilter ((== XMPP.IQResult) . XMPP.iqType) mreply)
			case helpMessage of
				Just msg -> sendMessage msg
				Nothing -> log "INVALID HELP MESSAGE" mreply
		Nothing ->
			case botHelp Nothing $ commandList componentJid Nothing componentJid from [] of
				Just msg -> sendMessage msg
				Nothing -> log "INVALID HELP MESSAGE" ()

adhocBotRunCommand :: (UIO.Unexceptional m) => DB.DB -> JID -> JID -> (XMPP.Message -> m ()) -> (XMPP.IQ -> UIO.UIO (STM (Maybe XMPP.IQ))) -> STM XMPP.Message -> JID -> Text -> [Element] -> m ()
adhocBotRunCommand db componentJid routeFrom sendMessage sendIQ getMessage from body cmdEls = do
	let (nodes, cmds) = unzip $ mapMaybe (\el -> (,) <$> attributeText (s"node") el <*> pure el) cmdEls

	case (snd <$> find (\(prefixes, _) -> Set.member (CI.mk body) prefixes) (zip (uniquePrefix nodes) cmds), registerShorthand body) of
		(_, Just gatewayJID) -> do
			mResult <- (atomicUIO =<<) $ UIO.lift $ sendIQ $ (emptyIQ IQSet) {
					iqFrom = Just routeFrom,
					iqTo = Just componentJid,
					iqPayload = Just $ Element (s"{http://jabber.org/protocol/commands}command") [(s"node", [ContentText ConfigureDirectMessageRoute.nodeName])] []
				}
			case attributeText (s"sessionid") =<< iqPayload =<< mResult of
				Just sessionid ->
					startWithIntro $ (emptyIQ IQSet) {
						iqFrom = Just routeFrom,
						iqTo = iqFrom =<< mResult,
						iqPayload = Just $ Element (s"{http://jabber.org/protocol/commands}command") [(s"node", [ContentText ConfigureDirectMessageRoute.nodeName]), (s"sessionid", [ContentText sessionid]), (s"action", [ContentText $ s"next"])] [
							NodeElement $ Element (fromString "{jabber:x:data}x") [
								(fromString "{jabber:x:data}type", [ContentText $ s"submit"])
							] [
								NodeElement $ Element (fromString "{jabber:x:data}field") [
									(fromString "{jabber:x:data}type", [ContentText $ s"jid-single"]),
									(fromString "{jabber:x:data}var", [ContentText $ s"gateway-jid"])
								] [
									NodeElement $ Element (fromString "{jabber:x:data}value") [] [NodeContent $ ContentText $ formatJID gatewayJID]
								]
							]
						]
					}
				Nothing -> sendHelp db componentJid sendMessage sendIQ from routeFrom
		(Just cmd, Nothing) ->
			startWithIntro $ (emptyIQ IQSet) {
				iqFrom = Just routeFrom,
				iqTo = parseJID =<< attributeText (s"jid") cmd,
				iqPayload = Just $ Element (s"{http://jabber.org/protocol/commands}command") [(s"node", [ContentText $ fromMaybe mempty $ attributeText (s"node") cmd])] []
			}
		(Nothing, Nothing) -> sendHelp db componentJid sendMessage sendIQ from routeFrom
	where
	startWithIntro cmdIQ =
		sendAndRespondTo (Just $ intercalate (s"\n") [
				s"You can leave something at the current value by saying 'next'.",
				s"You can return to the main menu by saying 'cancel' at any time."
			]) cmdIQ
	threadedMessage Nothing msg = msg
	threadedMessage (Just sessionid) msg = msg { messagePayloads = (Element (s"thread") [] [NodeContent $ ContentText sessionid]) : messagePayloads msg }
	continueExecution resultIQ responseBody
		| Just payload <- iqPayload resultIQ,
		  Just sessionid <- attributeText (s"sessionid") payload,
		  Just "executing" <- T.unpack <$> attributeText (s"status") payload = do
			let actions = listToMaybe $ isNamed(s"{http://jabber.org/protocol/commands}actions") =<< elementChildren payload
			-- The standard says if actions is present, with no "execute" attribute, that the default is "next"
			-- But if there is no actions, the default is "execute"
			let defaultAction = maybe (s"execute") (fromMaybe (s"next") . attributeText (s"execute")) actions
			let cmdIQ' = (emptyIQ IQSet) {
				iqFrom = Just routeFrom,
				iqTo = iqFrom resultIQ,
				iqPayload = Just $ Element (s"{http://jabber.org/protocol/commands}command") [(s"node", [ContentText $ fromMaybe mempty $ attributeText (s"node") payload]), (s"sessionid", [ContentText sessionid]), (s"action", [ContentText defaultAction])] responseBody
			}
			sendAndRespondTo Nothing cmdIQ'
	continueExecution _ _ = return ()
	sendAndRespondTo intro cmdIQ = do
		mcmdResult <- atomicUIO =<< UIO.lift (sendIQ cmdIQ)
		case mcmdResult of
			Just resultIQ
				| IQResult == iqType resultIQ,
				  Just payload <- iqPayload resultIQ,
				  [form] <- isNamed (s"{jabber:x:data}x") =<< elementChildren payload,
				  attributeText (s"type") form == Just (s"result") -> do
					let sendText = sendMessage . threadedMessage (attributeText (s"sessionid") payload) . mkSMS componentJid from
					sendText $ renderResultForm form
					continueExecution resultIQ []
				| IQResult == iqType resultIQ,
				  Just payload <- iqPayload resultIQ,
				  Just sessionid <- attributeText (s"sessionid") payload,
				  Just cmd <- attributeText (s"node") payload,
				  [form] <- isNamed (s"{jabber:x:data}x") =<< elementChildren payload -> do
					let cancelIQ = (emptyIQ IQSet) {
						iqFrom = Just routeFrom,
						iqTo = iqFrom resultIQ,
						iqPayload = Just $ Element (s"{http://jabber.org/protocol/commands}command") [(s"node", [ContentText cmd]), (s"sessionid", [ContentText sessionid]), (s"action", [ContentText $ s"cancel"])] []
					}
					let cancel = void . atomicUIO =<< UIO.lift (sendIQ cancelIQ)
					let sendText = sendMessage . threadedMessage (Just sessionid) . mkSMS componentJid from
					let cancelText = sendText . ((cmd ++ s" ") ++)
					forM_ intro sendText
					returnForm <- adhocBotAnswerForm sendText (withCancel sessionLifespan cancelText cancel getMessage) form
					continueExecution resultIQ [NodeElement returnForm]
				| IQResult == iqType resultIQ,
				  Just payload <- iqPayload resultIQ,
				  notes@(_:_) <- isNamed (s"{http://jabber.org/protocol/commands}note") =<< elementChildren payload -> do
					let sendText = sendMessage . threadedMessage (attributeText (s"sessionid") payload) . mkSMS componentJid from
					forM_ notes $
						sendText . mconcat . elementText
					if (attributeText (s"status") payload == Just (s"executing")) then do
						let actions = mapMaybe (actionFromXMPP . XML.nameLocalName . elementName) $ elementChildren =<< isNamed (s"{http://jabber.org/protocol/commands}actions") =<< elementChildren payload
						let sessionid = maybe [] (\sessid -> [(s"sessionid", [ContentText sessid])]) $ attributeText (s"sessionid") payload
						sendText $
							s"You can say one of: " ++
							(intercalate (s", ") $ map actionCmd (ActionCancel : actions))
						action <- waitForAction actions sendText (atomicUIO getMessage)
						let cmdIQ' = (emptyIQ IQSet) {
							iqFrom = Just routeFrom,
							iqTo = iqFrom resultIQ,
							iqPayload = Just $ Element (s"{http://jabber.org/protocol/commands}command") ([(s"node", [ContentText $ fromMaybe mempty $ attributeText (s"node") payload]), (s"action", [actionContent action])] ++ sessionid) []
						}
						sendAndRespondTo Nothing cmdIQ'
					else when (
							attributeText (s"status") payload == Just (s"completed") &&
							attributeText (s"node") payload == Just ConfigureDirectMessageRoute.nodeName &&
							all (\n -> attributeText (s"type") n /= Just (s"error")) notes
						) $
							sendHelp db componentJid sendMessage sendIQ from routeFrom
				| IQResult == iqType resultIQ,
				  [cmd] <- isNamed (s"{http://jabber.org/protocol/commands}command") =<< (justZ $ iqPayload resultIQ),
				  attributeText (s"status") cmd `elem` [Just (s"completed"), Just (s"canceled")] -> return ()
				| otherwise -> sendMessage $ mkSMS componentJid from (s"Command error")
			Nothing -> sendMessage $ mkSMS componentJid from (s"Command timed out")

adhocBotSession :: (UIO.Unexceptional m) => DB.DB -> JID -> (XMPP.Message -> m ()) -> (XMPP.IQ -> UIO.UIO (STM (Maybe XMPP.IQ))) -> STM XMPP.Message -> XMPP.Message-> m ()
adhocBotSession db componentJid sendMessage sendIQ getMessage message@(XMPP.Message { XMPP.messageFrom = Just from })
	| Just body <- getBody "jabber:component:accept" message = do
		maybeRoute <- (parseJID =<<) . (join . hush) <$> UIO.fromIO (DB.get db (DB.byJid from ["direct-message-route"]))
		case maybeRoute of
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
