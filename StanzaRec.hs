module StanzaRec (StanzaRec(..), mkStanzaRec, ensureId) where

import Prelude ()
import BasicPrelude
import qualified Data.UUID as UUID (toText)
import qualified Data.UUID.V1 as UUID (nextUUID)
import qualified Data.XML.Types as XML
import qualified Network.Protocol.XMPP as XMPP
import Network.Protocol.XMPP.Internal (Stanza(..))

import Util

data StanzaRec = StanzaRec (Maybe XMPP.JID) (Maybe XMPP.JID) (Maybe Text) (Maybe Text) [XML.Element] XML.Element deriving (Show)

instance Stanza StanzaRec where
	stanzaTo (StanzaRec to _ _ _ _ _) = to
	stanzaFrom (StanzaRec _ from _ _ _ _) = from
	stanzaID (StanzaRec _ _ sid _ _ _) = sid
	stanzaLang (StanzaRec _ _ _ lang _ _) = lang
	stanzaPayloads (StanzaRec _ _ _ _ payloads _) = payloads
	stanzaToElement (StanzaRec _ _ _ _ _ element) = element

mkStanzaRec :: (Stanza s) => s -> StanzaRec
mkStanzaRec x = StanzaRec (stanzaTo x) (stanzaFrom x) (stanzaID x) (stanzaLang x) (stanzaPayloads x) (stanzaToElement x)

ensureId :: StanzaRec -> IO StanzaRec
ensureId (StanzaRec to from Nothing lang payloads element) = do
	uuid <- (fmap.fmap) UUID.toText UUID.nextUUID
	return $ StanzaRec to from uuid lang payloads $ element {
			XML.elementAttributes =
				(s"id", [XML.ContentText $ fromMaybe mempty uuid]) :
				XML.elementAttributes element
		}
ensureId s = return s
