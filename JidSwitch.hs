module JidSwitch where

import Prelude ()
import BasicPrelude hiding (log)
import Data.UUID (UUID)
import qualified Data.UUID as UUID (toString, fromString)
import qualified Data.UUID.V1 as UUID (nextUUID)
import Data.XML.Types (Element(..), Node(NodeContent, NodeElement), Name(..), Content(ContentText), isNamed, hasAttributeText, elementText, elementChildren, attributeText)
import qualified Network.Protocol.XMPP as XMPP

import Util
import CommandAction
import StanzaRec

import qualified DB

backendNodeName :: Text
backendNodeName = s"https://ns.cheogram.com/sgx/jid-switch"

nodeName :: Text
nodeName = s"change jabber id"

newtype SessionID = SessionID UUID deriving (Ord, Eq, Show)

sessionIDFromText :: Text -> Maybe SessionID
sessionIDFromText txt = SessionID <$> UUID.fromString (textToString txt)

sessionIDToText :: SessionID -> Text
sessionIDToText (SessionID uuid) = fromString $ UUID.toString uuid

type FromJID = XMPP.JID
type Route = XMPP.JID

fromAssoc :: [(Text, Maybe Text)] -> Maybe (FromJID, Route)
fromAssoc assoc = (,) <$> (XMPP.parseJID =<< join (lookup (s"from") assoc)) <*> (XMPP.parseJID =<< join (lookup (s"route") assoc))

toAssoc :: FromJID -> Route -> [(Text, Maybe Text)]
toAssoc from route = [(s"from", Just $ bareTxt from), (s"route", Just $ bareTxt route)]

newSession :: IO SessionID
newSession = UUID.nextUUID >>= go
	where
	go (Just uuid) = return $ SessionID uuid
	go Nothing = do
		log "JidSwitch.newSession" "UUID generation failed"
		UUID.nextUUID >>= go

receiveIq componentJid setJidSwitch iq@(XMPP.IQ { XMPP.iqFrom = Just from, XMPP.iqPayload = Just realPayload })
	| [command] <- isNamed (fromString "{http://jabber.org/protocol/commands}command") =<< [realPayload],
	  Just action <- attributeText (s"action") command,
	  action `elem` [s"complete", s"execute"],
	  Just sid <- sessionIDFromText =<< attributeText (s"sessionid") command,
	  [form] <- isNamed (fromString "{jabber:x:data}x") =<< elementChildren command,
	  Just newJid <- XMPP.parseJID =<< getFormField form (s"new-jid") = do
		(from', newJid', _) <- setJidSwitch newJid
		return [
			mkStanzaRec $ mkSMS componentJid newJid $ concat [
				bareTxt from',
				s" has requested a Jabber ID change to ",
				bareTxt newJid',
				s". To complete this request send \"register\""
			],
			mkStanzaRec $ flip iqReply iq $ Just $ commandStage sid [] (s"completed") [
				Element (s"{http://jabber.org/protocol/commands}note") [
					(s"{http://jabber.org/protocol/commands}type", [ContentText $ s"info"])
				] [
					NodeContent $ ContentText $ s"Please check for a message on " ++ bareTxt newJid'
				]
			]]
	| [command] <- isNamed (fromString "{http://jabber.org/protocol/commands}command") =<< [realPayload],
	  Just sid <- sessionIDFromText =<< attributeText (s"sessionid") command =
		return [mkStanzaRec $ flip iqReply iq $ Just $ commandStage sid [ActionComplete] (s"canceled") []]
	| otherwise = do
		sid <- newSession
		return [mkStanzaRec $ stage1 sid iq]

stage1 sid iq = flip iqReply iq $ Just $ commandStage sid [ActionComplete] (s"executing") [
		Element (fromString "{jabber:x:data}x") [
			(fromString "{jabber:x:data}type", [ContentText $ s"form"])
		] [
			NodeElement $ Element (fromString "{jabber:x:data}title") [] [NodeContent $ ContentText $ s"Change Jabber ID"],
			NodeElement $ Element (fromString "{jabber:x:data}instructions") [] [
				NodeContent $ ContentText $ s"Enter the Jabber ID you'd like to move your account to"
			],
			NodeElement $ Element (fromString "{jabber:x:data}field") [
				(fromString "{jabber:x:data}type", [ContentText $ s"jid-single"]),
				(fromString "{jabber:x:data}var", [ContentText $ s"new-jid"]),
				(fromString "{jabber:x:data}label", [ContentText $ s"New Jabber ID"])
			] []
		]
	]

commandStage :: SessionID -> [Action] -> Text -> [Element] -> Element
commandStage sid acceptedActions status el = Element (s"{http://jabber.org/protocol/commands}command")
	[
		(s"{http://jabber.org/protocol/commands}node", [ContentText nodeName]),
		(s"{http://jabber.org/protocol/commands}sessionid", [ContentText $ sessionIDToText sid]),
		(s"{http://jabber.org/protocol/commands}status", [ContentText status])
	]
	(actions ++ map NodeElement el)
	where
	actions
		| null acceptedActions = []
		| otherwise = [
				NodeElement $ Element (s"{http://jabber.org/protocol/commands}actions") [
					(s"{http://jabber.org/protocol/commands}execute", [actionContent $ head acceptedActions])
				] (map NodeElement $ concatMap actionToEl acceptedActions)
			]
