module StanzaRec (StanzaRec(..), mkStanzaRec) where

import BasicPrelude
import qualified Data.XML.Types as XML
import qualified Network.Protocol.XMPP as XMPP
import Network.Protocol.XMPP.Internal (Stanza(..))

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

