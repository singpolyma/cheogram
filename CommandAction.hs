module CommandAction where

import Data.XML.Types as XML (Element(..), Node(NodeContent, NodeElement), Content(ContentText), isNamed, elementText, elementChildren, attributeText)

import qualified Data.Text as T
import qualified Data.XML.Types as XML

import Util

data Action = ActionNext | ActionPrev | ActionCancel | ActionComplete

actionContent :: Action -> Content
actionContent ActionNext     = ContentText $ s"next"
actionContent ActionPrev     = ContentText $ s"prev"
actionContent ActionCancel   = ContentText $ s"cancel"
actionContent ActionComplete = ContentText $ s"complete"

actionCmd :: Action -> T.Text
actionCmd ActionNext     = s"next"
actionCmd ActionPrev     = s"back"
actionCmd ActionCancel   = s"cancel"
actionCmd ActionComplete = s"finish"

actionFromXMPP :: T.Text -> Maybe Action
actionFromXMPP xmpp
	| xmpp == s"next"     = Just ActionNext
	| xmpp == s"prev"     = Just ActionPrev
	| xmpp == s"complete" = Just ActionComplete
	| otherwise           = Nothing

actionToEl :: Action -> [Element]
actionToEl ActionNext = [Element (s"{http://jabber.org/protocol/commands}next") [] []]
actionToEl ActionPrev = [Element (s"{http://jabber.org/protocol/commands}prev") [] []]
actionToEl ActionComplete = [Element (s"{http://jabber.org/protocol/commands}complete") [] []]
actionToEl ActionCancel = []
