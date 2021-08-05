module IQManager (iqManager) where

import Prelude ()
import BasicPrelude
import Control.Concurrent.STM (
		STM, TMVar, TVar, modifyTVar', newEmptyTMVar, newTVar, orElse,
		readTVar, takeTMVar, tryPutTMVar, writeTVar
	)
import Control.Concurrent.STM.Delay    (newDelay, waitDelay)
import UnexceptionalIO.Trans           (Unexceptional)
import qualified Data.Map.Strict       as Map
import qualified Network.Protocol.XMPP as XMPP
import qualified Data.UUID             as UUID
import qualified Data.UUID.V4          as UUID

import Util

type ResponseMap = Map.Map (Maybe Text) (TMVar XMPP.IQ)

iqSendTimeoutMicroseconds :: Int
iqSendTimeoutMicroseconds = 20 * 1000000

iqDefaultID :: (Unexceptional m) => XMPP.IQ -> m XMPP.IQ
iqDefaultID iq@XMPP.IQ { XMPP.iqID = Just _ } = return iq
iqDefaultID iq = do
	uuid <- fromIO_ UUID.nextRandom
	return $ iq {
			XMPP.iqID = Just $ UUID.toText uuid
		}

iqSenderUnexceptional :: (Unexceptional m) =>
	(XMPP.IQ -> m ())
	-> TVar ResponseMap
	-> XMPP.IQ
	-> m (STM (Maybe XMPP.IQ))
iqSenderUnexceptional sender responseMapVar iq = do
	iqToSend <- iqDefaultID iq
	timeout <- fromIO_ $ newDelay iqSendTimeoutMicroseconds
	iqResponseVar <- atomicUIO newEmptyTMVar
	atomicUIO $ modifyTVar' responseMapVar $
			Map.insert (XMPP.iqID iqToSend) iqResponseVar
	sender iqToSend
	return (
			(waitDelay timeout *> pure Nothing)
			`orElse`
			fmap Just (takeTMVar iqResponseVar)
		)

iqReceiver :: (Unexceptional m) => TVar ResponseMap -> XMPP.IQ -> m ()
iqReceiver responseMapVar receivedIQ
	| XMPP.iqType receivedIQ `elem` [XMPP.IQResult, XMPP.IQError] = do
		maybeIqResponseVar <- atomicUIO $ do
			responseMap <- readTVar responseMapVar
			let (maybeIqResponseVar, responseMap') =
				Map.updateLookupWithKey
				(const $ const Nothing)
				(XMPP.iqID receivedIQ) responseMap
			writeTVar responseMapVar $! responseMap'
			return maybeIqResponseVar
		forM_ maybeIqResponseVar $ \iqResponseVar ->
			atomicUIO $ tryPutTMVar iqResponseVar receivedIQ
	| otherwise = return () -- TODO: log or otherwise signal error?

iqManager :: (Unexceptional m1, Unexceptional m2, Unexceptional m3) =>
	(XMPP.IQ -> m2 ()) ->
	m1 (XMPP.IQ -> m2 (STM (Maybe XMPP.IQ)), XMPP.IQ -> m3 ())
iqManager sender = do
	responseMapVar <- atomicUIO $ newTVar Map.empty
	return (
			iqSenderUnexceptional sender responseMapVar,
			iqReceiver responseMapVar
		)
