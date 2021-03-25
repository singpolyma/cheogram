{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Config where

import Prelude ()
import BasicPrelude
import Network.HostAndPort (maybeHostAndPort)
import System.IO.Unsafe (unsafePerformIO)
import Control.Error (headZ)

import qualified Network.Socket as Socket
import qualified Database.Redis as Redis
import qualified Dhall
import qualified Dhall.Core as Dhall
import qualified Network.Protocol.XMPP as XMPP

import Util
import qualified RedisURL

data ServerConfig = ServerConfig { host :: Socket.HostName, port :: Socket.PortNumber } deriving (Dhall.Generic, Dhall.FromDhall, Show)

data Config = Config {
	componentJid :: XMPP.JID,
	server :: ServerConfig,
	secret :: Text,
	backend :: Text,
	did :: Text,
	registrationJid :: XMPP.JID,
	conferenceServers :: [Text],
	s5bListenOn :: [Socket.SockAddr],
	s5bAdvertise :: ServerConfig,
	jingleStore :: FilePath,
	jingleStoreURL :: Text,
	redis :: Redis.ConnectInfo,
	statsd :: ServerConfig,
	avatar :: Maybe FilePath
} deriving (Dhall.Generic, Dhall.FromDhall, Show)

instance Dhall.FromDhall XMPP.JID where
	autoWith _ = Dhall.Decoder {
			Dhall.extract = \(Dhall.TextLit (Dhall.Chunks _ txt)) ->
				maybe (Dhall.extractError $ s"Invalid JID") pure $ XMPP.parseJID txt,
			Dhall.expected = pure Dhall.Text
		}

instance Dhall.FromDhall Socket.PortNumber where
	autoWith _ = Dhall.Decoder {
			Dhall.extract = \(Dhall.NaturalLit nat) -> pure $ fromIntegral nat,
			Dhall.expected = pure Dhall.Natural
		}

instance Dhall.FromDhall Socket.SockAddr where
	autoWith _ = Dhall.Decoder {
			Dhall.extract = (\(Dhall.TextLit (Dhall.Chunks _ txt)) -> maybe (Dhall.extractError $ s"Invalid Socket Address") pure $ do
				Just (host, Just port) <- return $ maybeHostAndPort (textToString txt)
				-- This is not a great idea, but I'm lazy today and I really just want to parse IP addresses, which is a pure operation
				unsafePerformIO $ fmap (fmap Socket.addrAddress . headZ) $ Socket.getAddrInfo Nothing (Just host) (Just port)
			),
			Dhall.expected = pure Dhall.Text
		}

instance Dhall.FromDhall Redis.ConnectInfo where
	autoWith _ = Dhall.Decoder {
			Dhall.extract = (\(Dhall.TextLit (Dhall.Chunks _ txt)) ->
				either (Dhall.extractError . tshow) pure $ RedisURL.parseConnectInfo $ textToString txt
			),
			Dhall.expected = pure Dhall.Text
		}

