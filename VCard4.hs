module VCard4 (fetch) where

import Prelude ()
import BasicPrelude

import Control.Concurrent.STM (STM)
import UnexceptionalIO.Trans (Unexceptional)
import Network.Protocol.XMPP (IQ(..), emptyIQ, IQType(IQResult, IQGet), JID, parseJID)
import qualified Data.XML.Types as XML
import qualified Data.Bool.HT as HT

import Util

fetch :: (Unexceptional m) => (IQ -> m (STM (Maybe IQ))) -> JID -> Maybe JID -> m (Maybe XML.Element)
fetch sendIQ to from = do
	vcard4stm <- sendIQ (fetchVCard4 { iqTo = Just to, iqFrom = routeFrom })
	nickstm <- sendIQ (fetchNick { iqTo = Just to, iqFrom = routeFrom })
	vcardTempstm <- sendIQ (fetchVCardTemp { iqTo = Just to, iqFrom = routeFrom })

	vcard4 <- (parseVCard4Result =<<) <$> atomicUIO vcard4stm
	nick <- (parseNickResult =<<) <$> atomicUIO nickstm
	vcardTemp <- (parseVCardTempResult =<<) <$> atomicUIO vcardTempstm

	case concat $ mapMaybe (fmap XML.elementNodes) [vcard4, nick, vcardTemp] of
		els@(_:_) -> return $ Just $ XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}vcard") [] els
		_ -> return Nothing
	where
	routeFrom = parseJID =<< fmap (\jid -> bareTxt jid ++ s"/IQMANAGER") from

fetchVCard4 :: IQ
fetchVCard4 = (emptyIQ IQGet) {
		iqPayload = Just $ XML.Element (s"{http://jabber.org/protocol/pubsub}pubsub") [] [
			XML.NodeElement $ XML.Element (s"{http://jabber.org/protocol/pubsub}items")
				[(s"node", [XML.ContentText $ s"urn:xmpp:vcard4"])] []
		]
	}

parseVCard4Result :: IQ -> Maybe XML.Element
parseVCard4Result IQ { iqType = IQResult, iqPayload = Just payload } =
	listToMaybe $
	XML.isNamed (s"{urn:ietf:params:xml:ns:vcard-4.0}vcard") =<< XML.elementChildren =<<
	XML.isNamed (s"{http://jabber.org/protocol/pubsub}item") =<< XML.elementChildren =<<
	XML.isNamed (s"{http://jabber.org/protocol/pubsub}items") =<< XML.elementChildren =<<
	XML.isNamed (s"{http://jabber.org/protocol/pubsub}pubsub") payload
parseVCard4Result _ = Nothing

fetchNick :: IQ
fetchNick = (emptyIQ IQGet) {
		iqPayload = Just $ XML.Element (s"{http://jabber.org/protocol/pubsub}pubsub") [] [
			XML.NodeElement $ XML.Element (s"{http://jabber.org/protocol/pubsub}items")
				[(s"node", [XML.ContentText $ s"http://jabber.org/protocol/nick"])] []
		]
	}

parseNickResult :: IQ -> Maybe XML.Element
parseNickResult IQ { iqType = IQResult, iqPayload = Just payload } =
	fmap (\nickEl ->
		XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}vcard") [] [
			XML.NodeElement $ XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}nickname") [] [
				XML.NodeElement $ XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}text") []
					(map XML.NodeContent $ XML.elementContent nickEl)
			]
		]
	) $ listToMaybe $
	XML.isNamed (s"{http://jabber.org/protocol/nick}nick") =<< XML.elementChildren =<<
	XML.isNamed (s"{http://jabber.org/protocol/pubsub}item") =<< XML.elementChildren =<<
	XML.isNamed (s"{http://jabber.org/protocol/pubsub}items") =<< XML.elementChildren =<<
	XML.isNamed (s"{http://jabber.org/protocol/pubsub}pubsub") payload
parseNickResult _ = Nothing

fetchVCardTemp :: IQ
fetchVCardTemp = (emptyIQ IQGet) {
		iqPayload = Just $ XML.Element (s"{vcard-temp}vCard") [] []
	}

parseVCardTempResult :: IQ -> Maybe XML.Element
parseVCardTempResult IQ { iqType = IQResult, iqPayload = Just payload }
	| [vcard] <- XML.isNamed (s"{vcard-temp}vCard") payload =
		Just $ XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}vcard") [] $
		flip map (XML.elementChildren vcard) $ XML.NodeElement . \el ->
			HT.select el [
				(
					s"{vcard-temp}FN" == XML.elementName el,
					XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}fn") []
					[XML.NodeElement $ XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}text") []
						(map XML.NodeContent $ XML.elementContent el)]
				),
				(
					s"{vcard-temp}N" == XML.elementName el,
					XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}n") [] $
					flip map (XML.elementChildren el) $ XML.NodeElement . \subel ->
						HT.select el [
							(
								s"{vcard-temp}FAMILY" == XML.elementName subel,
								XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}surname") []
									(map XML.NodeContent $ XML.elementContent subel)
							),
							(
								s"{vcard-temp}GIVEN" == XML.elementName subel,
								XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}given") []
									(map XML.NodeContent $ XML.elementContent subel)
							),
							(
								s"{vcard-temp}MIDDLE" == XML.elementName subel,
								XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}additional") []
									(map XML.NodeContent $ XML.elementContent subel)
							),
							(
								s"{vcard-temp}PREFIX" == XML.elementName subel,
								XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}prefix") []
									(map XML.NodeContent $ XML.elementContent subel)
							),
							(
								s"{vcard-temp}SUFFIX" == XML.elementName subel,
								XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}suffix") []
									(map XML.NodeContent $ XML.elementContent subel)
							)
						]
				),
				(
					s"{vcard-temp}NICKNAME" == XML.elementName el,
					XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}nickname") []
					[XML.NodeElement $ XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}text") []
						(map XML.NodeContent $ XML.elementContent el)]
				),
				(
					s"{vcard-temp}PHOTO" == XML.elementName el,
					XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}photo") []
					[XML.NodeElement $ XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}uri") [] $
						case (
							XML.isNamed (s"{vcard-temp}TYPE") =<< XML.elementChildren el,
							XML.isNamed (s"{vcard-temp}EXTVAL") =<< XML.elementChildren el,
							XML.isNamed (s"{vcard-temp}BINVAL") =<< XML.elementChildren el
						) of
							(_, [extval], _) -> map XML.NodeContent $ XML.elementContent extval
							(typ, _, [binval]) -> map XML.NodeContent $
								(XML.ContentText (s"data:") : concatMap XML.elementContent typ) ++
								(XML.ContentText (s";base64,") : XML.elementContent binval)
							_ -> []
					]
				),
				(
					s"{vcard-temp}BDAY" == XML.elementName el,
					XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}bday") []
					[XML.NodeElement $ XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}date") []
						(map XML.NodeContent $ XML.elementContent el)]
				),
				(
					s"{vcard-temp}ADR" == XML.elementName el,
					let
						home = not $ null $ XML.isNamed (s"{vcard-temp}HOME") =<< XML.elementChildren el
						work = not $ null $ XML.isNamed (s"{vcard-temp}WORK") =<< XML.elementChildren el
						pref = not $ null $ XML.isNamed (s"{vcard-temp}PREF") =<< XML.elementChildren el
					in
					XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}adr") [] $
					((if home || work || pref then
						[
							XML.NodeElement $ XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}parameters") [] (map XML.NodeElement $ concat [
								if home || work then
									[XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}type") [] [
										XML.NodeElement $ XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}text") [] [
											XML.NodeContent $ XML.ContentText $ if home then s"home" else s"work"]]]
								else [],
								if pref then
									[XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}pref") [] [
										XML.NodeElement $ XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}integer") [] [
											XML.NodeContent $ XML.ContentText $ s"1"]]]
								else []
							])
						]
					else []) ++) $
					flip map (XML.elementChildren el) $ XML.NodeElement . \subel ->
						HT.select el [
							(
								s"{vcard-temp}POBOX" == XML.elementName subel,
								XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}pobox") []
									(map XML.NodeContent $ XML.elementContent subel)
							),
							(
								s"{vcard-temp}EXTADD" == XML.elementName subel,
								XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}ext") []
									(map XML.NodeContent $ XML.elementContent subel)
							),
							(
								s"{vcard-temp}STREET" == XML.elementName subel,
								XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}street") []
									(map XML.NodeContent $ XML.elementContent subel)
							),
							(
								s"{vcard-temp}LOCALITY" == XML.elementName subel,
								XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}locality") []
									(map XML.NodeContent $ XML.elementContent subel)
							),
							(
								s"{vcard-temp}REGION" == XML.elementName subel,
								XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}region") []
									(map XML.NodeContent $ XML.elementContent subel)
							),
							(
								s"{vcard-temp}PCODE" == XML.elementName subel,
								XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}code") []
									(map XML.NodeContent $ XML.elementContent subel)
							),
							(
								s"{vcard-temp}CTRY" == XML.elementName subel,
								XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}country") []
									(map XML.NodeContent $ XML.elementContent subel)
							)
						]
				),
				(
					s"{vcard-temp}TEL" == XML.elementName el,
					let
						home = not $ null $ XML.isNamed (s"{vcard-temp}HOME") =<< XML.elementChildren el
						work = not $ null $ XML.isNamed (s"{vcard-temp}WORK") =<< XML.elementChildren el
						text = not $ null $ XML.isNamed (s"{vcard-temp}TEXT") =<< XML.elementChildren el
						voice = not $ null $ XML.isNamed (s"{vcard-temp}VOICE") =<< XML.elementChildren el
						fax = not $ null $ XML.isNamed (s"{vcard-temp}FAX") =<< XML.elementChildren el
						cell = not $ null $ XML.isNamed (s"{vcard-temp}CELL") =<< XML.elementChildren el
						video = not $ null $ XML.isNamed (s"{vcard-temp}VIDEO") =<< XML.elementChildren el
						pager = not $ null $ XML.isNamed (s"{vcard-temp}pager") =<< XML.elementChildren el
						textphone = not $ null $ XML.isNamed (s"{vcard-temp}TEXTPHONE") =<< XML.elementChildren el
						pref = not $ null $ XML.isNamed (s"{vcard-temp}PREF") =<< XML.elementChildren el
					in
					XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}tel") [] $
					(if home || work || text || voice || fax || cell || video || pager || textphone || pref then
						[
							XML.NodeElement $ XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}parameters") [] (map XML.NodeElement $ concat [
								if home || work || text || voice || fax || cell || video || pager || textphone then
									[XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}type") [] [
										XML.NodeElement $ XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}text") [] [
											XML.NodeContent $ XML.ContentText $ HT.select (s"unknown") [
												(home, s"home"),
												(work, s"work"),
												(text, s"text"),
												(voice, s"voice"),
												(fax, s"fax"),
												(cell, s"cell"),
												(video, s"video"),
												(pager, s"pager"),
												(textphone, s"texphone")
											]]]]
								else [],
								if pref then
									[XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}pref") [] [
										XML.NodeElement $ XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}integer") [] [
											XML.NodeContent $ XML.ContentText $ s"1"]]]
								else []
							])
						]
					else []) ++
						[XML.NodeElement $ XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}uri") [] $
							XML.NodeContent (XML.ContentText $ s"tel:") :
							map XML.NodeContent (XML.elementContent =<< XML.isNamed (s"{vcard-temp}NUMBER") =<< XML.elementChildren el)]
				),
				(
					s"{vcard-temp}EMAIL" == XML.elementName el,
					let
						home = not $ null $ XML.isNamed (s"{vcard-temp}HOME") =<< XML.elementChildren el
						work = not $ null $ XML.isNamed (s"{vcard-temp}WORK") =<< XML.elementChildren el
						pref = not $ null $ XML.isNamed (s"{vcard-temp}PREF") =<< XML.elementChildren el
					in
					XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}email") [] $
					(if home || work || pref then
						[
							XML.NodeElement $ XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}parameters") [] (map XML.NodeElement $ concat [
								if home || work then
									[XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}type") [] [
										XML.NodeElement $ XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}text") [] [
											XML.NodeContent $ XML.ContentText $ HT.select (s"unknown") [
												(home, s"home"),
												(work, s"work")
											]]]]
								else [],
								if pref then
									[XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}pref") [] [
										XML.NodeElement $ XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}integer") [] [
											XML.NodeContent $ XML.ContentText $ s"1"]]]
								else []
							])
						]
					else []) ++
						[XML.NodeElement $ XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}text") [] $
							map XML.NodeContent (XML.elementContent =<< XML.isNamed (s"{vcard-temp}USERID") =<< XML.elementChildren el)]
				),
				(
					s"{vcard-temp}JABBERID" == XML.elementName el,
					XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}impp") []
					[XML.NodeElement $ XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}uri") []
						(XML.NodeContent (XML.ContentText $ s"xmpp:") : map XML.NodeContent (XML.elementContent el))]
				),
				(
					s"{vcard-temp}TZ" == XML.elementName el,
					XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}tz") []
					[XML.NodeElement $ XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}text") []
						(map XML.NodeContent (XML.elementContent el))]
				),
				(
					s"{vcard-temp}GEO" == XML.elementName el,
					XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}geo") []
					[XML.NodeElement $ XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}uri") [] $
						(XML.NodeContent (XML.ContentText $ s"geo:") :
						map XML.NodeContent (XML.elementContent =<< XML.isNamed (s"{vcard-temp}LAT") =<< XML.elementChildren el)) ++
						(XML.NodeContent (XML.ContentText $ s",") :
						map XML.NodeContent (XML.elementContent =<< XML.isNamed (s"{vcard-temp}LON") =<< XML.elementChildren el))]
				),
				(
					s"{vcard-temp}TITLE" == XML.elementName el,
					XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}title") []
					[XML.NodeElement $ XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}text") []
						(map XML.NodeContent (XML.elementContent el))]
				),
				(
					s"{vcard-temp}ROLE" == XML.elementName el,
					XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}role") []
					[XML.NodeElement $ XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}text") []
						(map XML.NodeContent (XML.elementContent el))]
				),
				(
					s"{vcard-temp}LOGO" == XML.elementName el,
					XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}logo") []
					[XML.NodeElement $ XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}uri") [] $
						case (
							XML.isNamed (s"{vcard-temp}TYPE") =<< XML.elementChildren el,
							XML.isNamed (s"{vcard-temp}EXTVAL") =<< XML.elementChildren el,
							XML.isNamed (s"{vcard-temp}BINVAL") =<< XML.elementChildren el
						) of
							(_, [extval], _) -> map XML.NodeContent $ XML.elementContent extval
							(typ, _, [binval]) -> map XML.NodeContent $
								(XML.ContentText (s"data:") : concatMap XML.elementContent typ) ++
								(XML.ContentText (s";base64,") : XML.elementContent binval)
							_ -> []
					]
				),
				(
					s"{vcard-temp}ORG" == XML.elementName el,
					XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}org") []
					[XML.NodeElement $ XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}text") [] $
						map XML.NodeContent (XML.elementContent =<< XML.isNamed (s"{vcard-temp}ORGNAME") =<< XML.elementChildren el)]
				),
				(
					s"{vcard-temp}CATEGORIES" == XML.elementName el,
					XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}categories") []
					[XML.NodeElement $ XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}text") []
						(map XML.NodeContent (XML.elementContent el))]
				),
				(
					s"{vcard-temp}NOTE" == XML.elementName el,
					XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}note") []
					[XML.NodeElement $ XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}text") []
						(map XML.NodeContent (XML.elementContent el))]
				),
				(
					s"{vcard-temp}PRODID" == XML.elementName el,
					XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}prodid") []
					[XML.NodeElement $ XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}text") []
						(map XML.NodeContent (XML.elementContent el))]
				),
				(
					s"{vcard-temp}REV" == XML.elementName el,
					XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}rev") []
					[XML.NodeElement $ XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}timestamp") []
						(map XML.NodeContent (XML.elementContent el))]
				),
				(
					s"{vcard-temp}SORT-STRING" == XML.elementName el,
					XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}sort-as") []
						(map XML.NodeContent (XML.elementContent el))
				),
				(
					s"{vcard-temp}SOUND" == XML.elementName el,
					XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}sound") []
					[XML.NodeElement $ XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}uri") [] $
						case (
							XML.isNamed (s"{vcard-temp}EXTVAL") =<< XML.elementChildren el,
							XML.isNamed (s"{vcard-temp}BINVAL") =<< XML.elementChildren el
						) of
							([extval], _) -> map XML.NodeContent $ XML.elementContent extval
							(_, [binval]) -> map XML.NodeContent $
								XML.ContentText (s"data:udio/basic;base64,") :
								XML.elementContent binval
							_ -> []
					]
				),
				(
					s"{vcard-temp}UID" == XML.elementName el,
					XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}uid") []
					[XML.NodeElement $ XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}uri") []
						(map XML.NodeContent (XML.elementContent el))]
				),
				(
					s"{vcard-temp}URL" == XML.elementName el,
					XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}url") []
					[XML.NodeElement $ XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}uri") []
						(map XML.NodeContent (XML.elementContent el))]
				),
				(
					s"{vcard-temp}KEY" == XML.elementName el,
					XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}key") []
					[XML.NodeElement $ XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}text") []
						(map XML.NodeContent (XML.elementContent el))]
				),
				(
					s"{vcard-temp}DESC" == XML.elementName el,
					XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}note") []
					[XML.NodeElement $ XML.Element (s"{urn:ietf:params:xml:ns:vcard-4.0}text") []
						(map XML.NodeContent (XML.elementContent el))]
				)
			]
parseVCardTempResult _ = Nothing

-- <iq type='get' from='+12266669991@component2.localhost' to='test\40singpolyma-beefy.lan@component.localhost' id='items1'> <pubsub xmlns='http://jabber.org/protocol/pubsub'> <items node='urn:xmpp:vcard4'/> </pubsub> </iq>
