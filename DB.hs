module DB (DB, Key(..), byJid, byNode, mk, get, getEnum, del, set, setEnum, sadd, srem, smembers, foldKeysM, hset, hdel, hgetall) where

import Prelude ()
import BasicPrelude

import GHC.Stack (HasCallStack)
import Network.Protocol.XMPP (JID(..), strNode)

import qualified Database.Redis as Redis
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T

import Util

data DB = DB {
	redis :: Redis.Connection
}

newtype Key = Key [String] deriving (Eq)

mk :: Redis.ConnectInfo -> IO DB
mk redisCI = DB <$> Redis.checkedConnect redisCI

-- | 0xFF is invalid everywhere in UTF8 and is the CBOR "break" byte
redisKey :: Key -> ByteString
redisKey (Key key) = intercalate (BS.singleton 0xff) $ map (encodeUtf8 . fromString) key

redisParseKey :: ByteString -> Key
redisParseKey = Key . map (textToString . T.decodeUtf8) . BS.split 0xff

-- | Run Redis action and if the reply is an error, send that to an IO exception
runRedisChecked :: (HasCallStack) => DB -> Redis.Redis (Either Redis.Reply a) -> IO a
runRedisChecked db action =
	either (ioError . userError . show) return =<<
	Redis.runRedis (redis db) action

get :: (HasCallStack) => DB -> Key -> IO (Maybe Text)
get db key = (fmap.fmap) T.decodeUtf8 $
	runRedisChecked db (Redis.get (redisKey key))

getEnum :: (HasCallStack, Enum a) => DB -> Key -> IO (Maybe a)
getEnum db key = (fmap.fmap) (toEnum . read . T.decodeUtf8) $
	runRedisChecked db (Redis.get (redisKey key))

del :: (HasCallStack) => DB -> Key -> IO ()
del db key = void $ runRedisChecked db $ Redis.del [redisKey key]

set :: (HasCallStack) => DB -> Key -> Text -> IO ()
set db key val = do
	Redis.Ok <- runRedisChecked db $ Redis.set (redisKey key) (encodeUtf8 val)
	return ()

setEnum :: (HasCallStack, Enum a) => DB -> Key -> a -> IO ()
setEnum db key val = do
	Redis.Ok <- runRedisChecked db $ Redis.set (redisKey key) (encodeUtf8 $ tshow $ fromEnum val)
	return ()

sadd :: (HasCallStack) => DB -> Key -> [Text] -> IO ()
sadd _ _ [] = return ()
sadd db key new =
	void $ runRedisChecked db $ Redis.sadd (redisKey key) (map encodeUtf8 new)

srem :: (HasCallStack) => DB -> Key -> [Text] -> IO ()
srem db key toremove =
	void $ runRedisChecked db $ Redis.srem (redisKey key) (map encodeUtf8 toremove)

smembers :: (HasCallStack) => DB -> Key -> IO [Text]
smembers db key =
	map T.decodeUtf8 <$> runRedisChecked db (Redis.smembers (redisKey key))

-- | Encode Just txt as UTF8 txt and Nothing as "\xf6"
--   This is invalid UTF8, so there is no overlap. It is also the CBOR value for null
redisMaybe :: Maybe Text -> ByteString
redisMaybe (Just txt) = encodeUtf8 txt
redisMaybe Nothing = BS.singleton 0xf6

readRedisMaybe :: ByteString -> Maybe Text
readRedisMaybe bytes
	| bytes == BS.singleton 0xf6 = Nothing
	| otherwise = Just $ T.decodeUtf8 bytes

hset :: (HasCallStack) => DB -> Key -> [(Text, Maybe Text)] -> IO ()
hset _ _ [] = return ()
hset db key newitems =
	void $ runRedisChecked db (Redis.hmset (redisKey key) (map (encodeUtf8 *** redisMaybe) newitems))

hdel :: (HasCallStack) => DB -> Key -> [Text] -> IO ()
hdel db key toremove =
	void $ runRedisChecked db (Redis.hdel (redisKey key) (map encodeUtf8 toremove))

hgetall :: (HasCallStack) => DB -> Key -> IO [(Text, Maybe Text)]
hgetall db key =
	map (T.decodeUtf8 *** readRedisMaybe) <$>
		runRedisChecked db (Redis.hgetall (redisKey key))

foldKeysM :: (HasCallStack) => DB -> Key -> b -> (b -> Key -> IO b) -> IO b
foldKeysM db (Key prefix) z f = go Redis.cursor0 z
	where
	pattern = redisKey $ Key $ prefix ++ ["*"]
	go cursor acc = do
		(cursor', keys) <- runRedisChecked db $ Redis.scanOpts cursor (Redis.ScanOpts (Just pattern) (Just 100))
		acc' <- foldM f acc $ map redisParseKey keys
		if cursor' == Redis.cursor0 then return acc' else
			go cursor' acc'

byJid :: JID -> [String] -> Key
byJid jid subkey = Key $ (textToString $ bareTxt jid) : subkey

-- | Used when we know the JID is @cheogram.com, for example
--   So usually this is ByTel, really
byNode :: (HasCallStack) => JID -> [String] -> Key
byNode (JID { jidNode = Just node }) subkey =
	Key $ (textToString $ strNode node) : subkey
byNode jid _ = error $ "JID without node used in byNode: " ++ show jid
