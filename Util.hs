module Util where

import Prelude ()
import BasicPrelude
import Data.Char (isDigit)
import Control.Applicative (many)
import Control.Error (hush)
import Data.Time (getCurrentTime)
import Data.XML.Types (Element(..), Node(NodeElement), isNamed, elementText, elementChildren, attributeText)
import Crypto.Random (getSystemDRG, withRandomBytes)
import Data.ByteString.Base58 (bitcoinAlphabet, encodeBase58)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.Protocol.XMPP as XMPP
import qualified Data.Attoparsec.Text as Atto

log :: (Show a, MonadIO m) => String -> a -> m ()
log tag x = liftIO $ do
	time <- getCurrentTime
	putStr (show time ++ s" " ++ fromString tag ++ s" :: ") >> print x >> putStrLn mempty

s :: (IsString a) => String -> a
s = fromString

infixr .:
(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(.:) = (.).(.)

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

parsePhoneContext :: Text -> Maybe (Text, Text)
parsePhoneContext txt = hush $ Atto.parseOnly (
		(,) <$> Atto.takeWhile isDigit <* Atto.string (s";phone-context=") <*> Atto.takeTill (Atto.inClass " ;")
		<* Atto.endOfInput
	) txt

bareTxt (XMPP.JID (Just node) domain _) = mconcat [XMPP.strNode node, s"@", XMPP.strDomain domain]
bareTxt (XMPP.JID Nothing domain _) = XMPP.strDomain domain

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
