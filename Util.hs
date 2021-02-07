module Util where

import Prelude ()
import BasicPrelude
import Control.Concurrent
import Control.Concurrent.STM (STM, atomically)
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

log :: (Show a, Unexceptional m) => String -> a -> m ()
log tag x = fromIO_ $ do
	time <- getCurrentTime
	putStr (tshow time ++ s" " ++ fromString tag ++ s" :: ") >> print x >> putStrLn mempty

s :: (IsString a) => String -> a
s = fromString

fromIO_ :: (Unexceptional m) => IO a -> m a
fromIO_ = fmap (either absurd id) . UIO.fromIO' (error . show)

atomicUIO :: (Unexceptional m) => STM a -> m a
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

autolinkRegex :: PCRE.Regex
autolinkRegex = PCRE.compile (encodeUtf8 $ s"((http|https)://)?([a-z0-9-]+\\.)?[a-z0-9-]+(\\.[a-z]{2,6}){1,3}(/[a-z0-9.,_/~#&=;%+?-]*)?") [PCRE.caseless, PCRE.dotall]

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

mkSMS :: XMPP.JID -> XMPP.JID -> Text -> XMPP.Message
mkSMS from to txt = (XMPP.emptyMessage XMPP.MessageChat) {
	XMPP.messageTo = Just to,
	XMPP.messageFrom = Just from,
	XMPP.messagePayloads = [XML.Element (fromString "{jabber:component:accept}body") [] [XML.NodeContent $ XML.ContentText txt]]
}

castException :: (Ex.Exception e1, Ex.Exception e2) => e1 -> Maybe e2
castException = Ex.fromException . Ex.toException

-- Re-throws all by ThreadKilled async to parent thread
-- Makes sync child exceptions async in parent, which is a bit sloppy
forkXMPP :: XMPP.XMPP () -> XMPP.XMPP ThreadId
forkXMPP kid = do
	parent <- liftIO myThreadId
	session <- XMPP.getSession
	liftIO $ forkFinally
		(void $ XMPP.runXMPP session kid)
		(either (handler parent) (const $ return ()))
	where
	handler parent e
		| Just Ex.ThreadKilled <- castException e = return ()
		| otherwise = throwTo parent e
