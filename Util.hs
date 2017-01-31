module Util where

import Prelude ()
import BasicPrelude

import Data.Time (getCurrentTime)
import Data.XML.Types (Element(..), Node(NodeElement), isNamed, elementText, elementChildren, attributeText)
import qualified Data.Text as T
import qualified Network.Protocol.XMPP as XMPP

log :: (Show a, MonadIO m) => String -> a -> m ()
log tag x = liftIO $ do
	time <- getCurrentTime
	putStr (show time ++ s" " ++ fromString tag ++ s" :: ") >> print x >> putStrLn mempty

s :: (IsString a) => String -> a
s = fromString

escapeJid :: Text -> Text
escapeJid txt = T.concatMap (\char ->
		case char of
			' ' -> s"\\20"
			'"' -> s"\\22"
			'&' -> s"\\26"
			'\'' -> s"\\27"
			'/' -> s"\\2f"
			':' -> s"\\3a"
			'<' -> s"\\3c"
			'>' -> s"\\3e"
			'@' -> s"\\40"
			'\\' -> s"\\5c"
			c -> T.singleton c
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
