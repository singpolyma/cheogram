module IqHandler (main, register) where

import Prelude ()
import BasicPrelude hiding (log, forM_)
import Data.Foldable (forM_)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Loops (iterateM_)
import qualified Network.Protocol.XMPP as XMPP
import qualified Data.UUID as UUID (toString)
import qualified Data.UUID.V1 as UUID (nextUUID)
import qualified Data.Map as Map

import Util

data IqHandlerCommand =
	RegisterHandler Text (XMPP.IQ -> IO ()) |
	HandleIQ XMPP.IQ |
	RemoveHandler Text

register :: (Text -> (XMPP.IQ -> IO ()) -> IO ()) -> XMPP.IQ -> (XMPP.IQ -> IO()) -> IO XMPP.IQ
register registerHandler iq handler = UUID.nextUUID >>= go
	where
	uuidToId uuid = fromString $ "CHEOGRAM/" ++ UUID.toString uuid
	go (Just uuid) = do
		registerHandler (uuidToId uuid) handler
		return (iq { XMPP.iqID = Just (uuidToId uuid) })
	go Nothing = do
		log "IqHandler.register: UUID generation failed" iq
		UUID.nextUUID >>= go

main :: IO (Text -> (XMPP.IQ -> IO ()) -> IO (), XMPP.IQ -> IO ())
main = do
	commands <- newTQueueIO
	_ <- forkIO $ iterateM_ (thread commands) Map.empty
	return (
			atomically .: writeTQueue commands .: RegisterHandler,
			atomically . writeTQueue commands . HandleIQ
		)

thread :: TQueue IqHandlerCommand -> Map Text (XMPP.IQ -> IO ()) -> IO (Map Text (XMPP.IQ -> IO ()))
thread commands handlers = do
	command <- atomically $ readTQueue commands
	case command of
		RemoveHandler iqID -> return $! Map.delete iqID handlers
		RegisterHandler iqID handler -> do
			scheduleCommand 5000000 $ RemoveHandler iqID
			return $! Map.insert iqID handler handlers
		HandleIQ iq@(XMPP.IQ {
			XMPP.iqType = typ,
			XMPP.iqID = Just iqID
		}) | typ `elem` [XMPP.IQResult, XMPP.IQError] -> do
			let (handler, handlers') = lookupAndDelete iqID handlers
			forM_ handler ($iq)
			return handlers'
		HandleIQ iq -> do
			log "IQHandler.thread: bad stanza" iq
			return handlers
	where
	scheduleCommand wait cmd = void $ forkIO $ do
		threadDelay wait
		atomically $ writeTQueue commands cmd

lookupAndDelete :: (Ord k) => k -> Map k v -> (Maybe v, Map k v)
lookupAndDelete = Map.updateLookupWithKey (\_ _ -> Nothing)
