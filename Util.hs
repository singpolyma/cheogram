module Util where

import Prelude ()
import BasicPrelude
import Control.Concurrent
import Control.Concurrent.STM (STM, atomically)
import System.Exit (ExitCode)
import GHC.Stack (HasCallStack)
import Data.Word (Word16)
import Data.Bits (shiftL, (.|.))
import Data.Char (isDigit)
import Control.Applicative (many)
import Control.Error (hush)
import Data.Time (getCurrentTime)
import Data.XML.Types as XML (Name(Name), Element(..), Node(NodeElement, NodeContent), Content(ContentText), isNamed, elementText, elementChildren, attributeText)
import Crypto.Random (getSystemDRG, withRandomBytes)
import Data.ByteString.Base58 (bitcoinAlphabet, encodeBase58)
import Data.Digest.Pure.SHA (sha1, bytestringDigest)
import Data.Void (absurd)
import UnexceptionalIO (Unexceptional)
import qualified UnexceptionalIO       as UIO
import qualified Control.Exception as Ex
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.Protocol.XMPP as XMPP
import qualified Data.Attoparsec.Text as Atto
import qualified Data.ByteString.Lazy as LZ
import qualified Text.Regex.PCRE.Light as PCRE

instance Unexceptional XMPP.XMPP where
	lift = liftIO . UIO.lift

log :: (HasCallStack, Show a, Unexceptional m) => String -> a -> m ()
log tag x = fromIO_ $ do
	time <- getCurrentTime
	putStr (tshow time ++ s" " ++ fromString tag ++ s" :: ") >> print x >> putStrLn mempty

s :: (IsString a) => String -> a
s = fromString

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

fromIO_ :: (HasCallStack, Unexceptional m) => IO a -> m a
fromIO_ = fmap (either absurd id) . UIO.fromIO' (error . show)

atomicUIO :: (HasCallStack, Unexceptional m) => STM a -> m a
atomicUIO = fromIO_ . atomically

escapeJid :: Text -> Text
escapeJid txt = mconcat result
	where
	Right result = Atto.parseOnly (many (
			slashEscape <|>
			replace ' ' "\\20" <|>
			replace '"' "\\22" <|>
			replace '&' "\\26" <|>
			replace '\'' "\\27" <|>
			replace '/' "\\2f" <|>
			replace ':' "\\3a" <|>
			replace '<' "\\3c" <|>
			replace '>' "\\3e" <|>
			replace '@' "\\40" <|>
			fmap T.singleton Atto.anyChar
		) <* Atto.endOfInput) txt
	replace c str = Atto.char c *> pure (fromString str)
	-- XEP-0106 says to only escape \ when absolutely necessary
	slashEscape =
		fmap (s"\\5c"++) $
		Atto.char '\\' *> Atto.choice escapes
	escapes = map (Atto.string . fromString) [
			"20", "22", "26", "27", "2f", "3a", "3c", "3e", "40", "5c"
		]

unescapeJid :: Text -> Text
unescapeJid txt = fromString result
	where
	Right result = Atto.parseOnly (many (
			(Atto.char '\\' *> Atto.choice unescapes) <|>
			Atto.anyChar
		) <* Atto.endOfInput) txt
	unescapes = map (\(str, c) -> Atto.string (fromString str) *> pure c) [
			("20", ' '), ("22", '"'), ("26", '&'), ("27", '\''), ("2f", '/'), ("3a", ':'), ("3c", '<'), ("3e", '>'), ("40", '@'), ("5c", '\\')
		]

-- Matches any URL-ish text, but not x@x.tld forms
autolinkRegex :: PCRE.Regex
autolinkRegex = PCRE.compile (encodeUtf8 $ s"(?<!@)(?<=\\b)(?:((http|https)://)?([a-z0-9-]+\\.)?[a-z0-9-]+(\\.[a-z]{2,6}){1,3}(/[a-z0-9.,_/~#&=;%+?-]*)?)") [PCRE.caseless, PCRE.dotall]

sanitizeSipLocalpart :: Text -> Maybe Text
sanitizeSipLocalpart localpart
	| Just ('+', tel) <- T.uncons candidate,
	  T.all isDigit tel = Just candidate
	| T.length candidate < 3 =
		Just $ s"13;phone-context=anonymous.phone-context.soprani.ca"
	| candidate == s"Restricted" =
		Just $ s"14;phone-context=anonymous.phone-context.soprani.ca"
	| candidate == s"anonymous" =
		Just $ s"15;phone-context=anonymous.phone-context.soprani.ca"
	| candidate == s"Anonymous" =
		Just $ s"16;phone-context=anonymous.phone-context.soprani.ca"
	| candidate == s"unavailable" =
		Just $ s"17;phone-context=anonymous.phone-context.soprani.ca"
	| candidate == s"Unavailable" =
		Just $ s"18;phone-context=anonymous.phone-context.soprani.ca"
	| otherwise = Nothing
	where
	candidate = fst $ T.breakOn (s"@") $ unescapeJid localpart

showAvailableness :: String -> Word8
showAvailableness "chat" = 5
showAvailableness ""     = 4
showAvailableness "away" = 3
showAvailableness "dnd"  = 2
showAvailableness "xa"   = 1
showAvailableness _      = 0

priorityAvailableness :: Integer -> Word8
priorityAvailableness priority
	| priority > 127 = 0xff
	| priority < -128 = 0x00
	| otherwise = fromIntegral (priority + 128)

availableness :: Text -> Integer -> Word16
availableness sshow priority =
	(fromIntegral (showAvailableness (textToString sshow)) `shiftL` 8) .|.
	(fromIntegral (priorityAvailableness priority))

parsePhoneContext :: Text -> Maybe (Text, Text)
parsePhoneContext txt = hush $ Atto.parseOnly (
		(,) <$> Atto.takeWhile isDigit <* Atto.string (s";phone-context=") <*> Atto.takeTill (Atto.inClass " ;")
		<* Atto.endOfInput
	) txt

bareTxt :: XMPP.JID -> Text
bareTxt (XMPP.JID (Just node) domain _) = mconcat [XMPP.strNode node, s"@", XMPP.strDomain domain]
bareTxt (XMPP.JID Nothing domain _) = XMPP.strDomain domain

getFormField :: XML.Element -> Text -> Maybe Text
getFormField form var =
		listToMaybe $ mapMaybe (\node ->
			case node of
				NodeElement el
					| elementName el == s"{jabber:x:data}field" &&
					  (attributeText (s"{jabber:x:data}var") el == Just var ||
					  attributeText (s"var") el == Just var) ->
						Just $ mconcat $
						elementText =<< isNamed (s"{jabber:x:data}value") =<< elementChildren el
				_ -> Nothing
		) (elementNodes form)

genToken :: Int -> IO Text
genToken n = do
	g <- getSystemDRG
	return $ fst $ withRandomBytes g n (T.decodeUtf8 . encodeBase58 bitcoinAlphabet)

child :: (XMPP.Stanza s) => Name -> s -> Maybe Element
child name = listToMaybe .
	(isNamed name <=< XMPP.stanzaPayloads)

attrOrBlank :: XML.Name -> XML.Element -> Text
attrOrBlank name el = fromMaybe mempty $ XML.attributeText name el

discoCapsIdentities :: XML.Element -> [Text]
discoCapsIdentities query =
	sort $
	map (\identity -> mconcat $ intersperse (s"/") [
		attrOrBlank (s"category") identity,
		attrOrBlank (s"type") identity,
		attrOrBlank (s"xml:lang") identity,
		attrOrBlank (s"name") identity
	]) $
	XML.isNamed (s"{http://jabber.org/protocol/disco#info}identity") =<<
		XML.elementChildren query

discoVars :: XML.Element -> [Text]
discoVars query =
	sort $
	mapMaybe (XML.attributeText (s"var")) $
	XML.isNamed (s"{http://jabber.org/protocol/disco#info}feature") =<<
		XML.elementChildren query

data DiscoForm = DiscoForm Text [(Text, [Text])] deriving (Show, Ord, Eq)

oneDiscoForm :: XML.Element -> DiscoForm
oneDiscoForm form =
	DiscoForm form_type (filter ((/= s"FORM_TYPE") . fst) fields)
	where
	form_type = mconcat $ fromMaybe [] $ lookup (s"FORM_TYPE") fields
	fields = sort $ map (\field ->
			(
				attrOrBlank (s"var") field,
				sort (map (mconcat . XML.elementText) $ XML.isNamed (s"{jabber:x:data}value") =<< XML.elementChildren form)
			)
		) $
		XML.isNamed (s"{jabber:x:data}field") =<<
			XML.elementChildren form

discoForms :: XML.Element -> [DiscoForm]
discoForms query =
	sort $
	map oneDiscoForm $
	XML.isNamed (s"{jabber:x:data}x") =<<
		XML.elementChildren query

discoCapsForms :: XML.Element -> [Text]
discoCapsForms query =
	concatMap (\(DiscoForm form_type fields) ->
		form_type : concatMap (uncurry (:)) fields
	) (discoForms query)

discoToCaps :: XML.Element -> Text
discoToCaps query =
	(mconcat $ intersperse (s"<") (discoCapsIdentities query ++ discoVars query ++ discoCapsForms query)) ++ s"<"

discoToCapsHash :: XML.Element -> ByteString
discoToCapsHash query =
	LZ.toStrict $ bytestringDigest $ sha1 $ LZ.fromStrict $ T.encodeUtf8 $ discoToCaps query

getBody :: String -> XMPP.Message -> Maybe Text
getBody ns = listToMaybe . fmap (mconcat . XML.elementText) . (XML.isNamed (XML.Name (fromString "body") (Just $ fromString ns) Nothing) <=< XMPP.messagePayloads)

getThread :: String -> XMPP.Message -> Maybe Text
getThread ns = listToMaybe . fmap (mconcat . XML.elementText) . (XML.isNamed (XML.Name (fromString "thread") (Just $ fromString ns) Nothing) <=< XMPP.messagePayloads)

mkSMS :: XMPP.JID -> XMPP.JID -> Text -> XMPP.Message
mkSMS from to txt = (XMPP.emptyMessage XMPP.MessageChat) {
	XMPP.messageTo = Just to,
	XMPP.messageFrom = Just from,
	XMPP.messagePayloads = [mkElement (s"{jabber:component:accept}body") txt]
}

castException :: (Ex.Exception e1, Ex.Exception e2) => e1 -> Maybe e2
castException = Ex.fromException . Ex.toException

-- Re-throws all by ThreadKilled async to parent thread
-- Makes sync child exceptions async in parent, which is a bit sloppy
forkXMPP :: XMPP.XMPP () -> XMPP.XMPP ThreadId
forkXMPP kid = do
	parent <- liftIO myThreadId
	forkFinallyXMPP kid (either (handler parent) (const $ return ()))
	where
	handler parent e
		| Just Ex.ThreadKilled <- castException e = return ()
		| Just (Ex.SomeAsyncException _) <- castException e = throwTo parent e
		| Just e <- castException e = throwTo parent (e :: ExitCode)
		| otherwise = throwTo parent (ChildThreadError e)

forkFinallyXMPP :: XMPP.XMPP () -> (Either SomeException () -> IO ()) -> XMPP.XMPP ThreadId
forkFinallyXMPP kid handler = do
	session <- XMPP.getSession
	liftIO $ forkFinally (void $ XMPP.runXMPP session kid) handler

newtype ChildThreadError = ChildThreadError SomeException deriving (Show, Typeable)

instance Ex.Exception ChildThreadError where
	toException = Ex.asyncExceptionToException
	fromException = Ex.asyncExceptionFromException

mkElement :: XML.Name -> Text -> XML.Element
mkElement name txt = XML.Element name [] [XML.NodeContent $ XML.ContentText txt]

nickname :: Text -> XML.Element
nickname nick = XML.Element (s"{http://jabber.org/protocol/nick}nick") [] [
		XML.NodeContent $ XML.ContentText nick
	]

addNickname :: Text -> XMPP.Message -> XMPP.Message
addNickname nick m@(XMPP.Message { XMPP.messagePayloads = p }) = m {
		XMPP.messagePayloads = (nickname nick) : p
	}

mapReceivedMessageM :: (Applicative f) =>
	  (XMPP.Message -> f XMPP.Message)
	-> XMPP.ReceivedStanza
	-> f XMPP.ReceivedStanza
mapReceivedMessageM f (XMPP.ReceivedMessage m) = XMPP.ReceivedMessage <$> f m
mapReceivedMessageM _ s = pure s

iqReply :: Maybe XML.Element -> XMPP.IQ -> XMPP.IQ
iqReply payload iq = iq {
	XMPP.iqType = XMPP.IQResult,
	XMPP.iqFrom = XMPP.iqTo iq,
	XMPP.iqTo = XMPP.iqFrom iq,
	XMPP.iqPayload = payload
}

queryCommandList' :: XMPP.JID -> XMPP.JID -> XMPP.IQ
queryCommandList' to from = (XMPP.emptyIQ XMPP.IQGet) {
	XMPP.iqTo = Just to,
	XMPP.iqFrom = Just from,
	XMPP.iqPayload = Just $ XML.Element (s"{http://jabber.org/protocol/disco#items}query") [
		(s"{http://jabber.org/protocol/disco#items}node", [XML.ContentText $ s"http://jabber.org/protocol/commands"])
	] []
}

queryDiscoWithNode' :: Maybe Text -> XMPP.JID -> XMPP.JID -> XMPP.IQ
queryDiscoWithNode' node to from =
	(XMPP.emptyIQ XMPP.IQGet) {
		XMPP.iqTo = Just to,
		XMPP.iqFrom = Just from,
		XMPP.iqPayload = Just $ XML.Element
			(s"{http://jabber.org/protocol/disco#info}query")
			(map (\node -> (s"{http://jabber.org/protocol/disco#info}node", [XML.ContentText node])) $ maybeToList node)
			[]
	}

parseBool :: Text -> Maybe Bool
parseBool input
	| s"true" == input = Just True
	| s"1" == input = Just True
	| s"false" == input = Just False
	| s"0" == input = Just False
	| otherwise = Nothing

hasLocked :: String -> IO a -> IO a
hasLocked msg action =
  action `Ex.catches`
  [ Ex.Handler $ \exc@Ex.BlockedIndefinitelyOnMVar -> Util.log "[MVar]" msg >> Ex.throwIO exc
  , Ex.Handler $ \exc@Ex.BlockedIndefinitelyOnSTM -> Util.log "[STM]" msg >> Ex.throwIO exc
  ]
