module DB (DB, Key(..), byJid, byNode, mk, get, getEnum, del, set, setEnum, sadd, srem, smembers, foldKeysM, hset, hdel, hgetall) where

import Prelude ()
import BasicPrelude

import GHC.Stack (HasCallStack)
import Control.Error (readZ)
import Network.Protocol.XMPP (JID(..), strNode)

import qualified Database.TokyoCabinet as TC
import qualified Database.Redis as Redis
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Util

data DB = DB {
	tcdb :: TC.HDB,
	redis :: Redis.Connection
}

newtype Key = Key [String] deriving (Eq)

openTokyoCabinet :: (TC.TCDB a) => String -> IO a
openTokyoCabinet pth = TC.runTCM $ do
	db <- TC.new
	True <- TC.open db pth [TC.OREADER, TC.OWRITER, TC.OCREAT]
	return db

mk :: String -> Redis.ConnectInfo -> IO DB
mk tcPath redisCI = do
	tcdb <- openTokyoCabinet tcPath
	redis <- Redis.checkedConnect redisCI
	return $ DB tcdb redis

-- | 0xFF is invalid everywhere in UTF8 and is the CBOR "break" byte
redisKey :: Key -> ByteString
redisKey (Key key) = intercalate (BS.singleton 0xff) $ map (encodeUtf8 . fromString) key

redisParseKey :: ByteString -> Key
redisParseKey = Key . map (textToString . T.decodeUtf8) . BS.split 0xff

tcKey :: Key -> String
tcKey (Key key) = intercalate "\0" key

tcParseKey :: String -> Key
tcParseKey str = Key $ map textToString $ T.split (=='\0') $ fromString str

-- | Run Redis action and if the reply is an error, send that to an IO exception
runRedisChecked :: (HasCallStack) => DB -> Redis.Redis (Either Redis.Reply a) -> IO a
runRedisChecked db action =
	either (ioError . userError . show) return =<<
	Redis.runRedis (redis db) action

get :: (HasCallStack) => DB -> Key -> IO (Maybe Text)
get db key = maybe
	(fmap fromString <$> (TC.runTCM $ TC.get (tcdb db) $ tcKey key))
	(return . Just . T.decodeUtf8) =<<
	runRedisChecked db (Redis.get (redisKey key))

getEnum :: (HasCallStack, Enum a) => DB -> Key -> IO (Maybe a)
getEnum db key = (fmap.fmap) toEnum $ maybe
	(TC.runTCM $ TC.get (tcdb db) $ tcKey key)
	(return . Just . read . T.decodeUtf8) =<<
	runRedisChecked db (Redis.get (redisKey key))

del :: (HasCallStack) => DB -> Key -> IO ()
del db key = do
	void $ runRedisChecked db $ Redis.del [redisKey key]
	-- May return false if key is not present, but that's fine
	void $ TC.runTCM $ TC.out (tcdb db) $ tcKey key
	return ()

set :: (HasCallStack) => DB -> Key -> Text -> IO ()
set db key val = do
	Redis.Ok <- runRedisChecked db $ Redis.set (redisKey key) (encodeUtf8 val)
	True <- TC.runTCM $ TC.put (tcdb db) (tcKey key) (textToString val)
	return ()

setEnum :: (HasCallStack, Enum a) => DB -> Key -> a -> IO ()
setEnum db key val = do
	Redis.Ok <- runRedisChecked db $ Redis.set (redisKey key) (encodeUtf8 $ tshow $ fromEnum val)
	True <- TC.runTCM $ TC.put (tcdb db) (tcKey key) (fromEnum val)
	return ()

sadd :: (HasCallStack) => DB -> Key -> [Text] -> IO ()
sadd _ _ [] = return ()
sadd db key new = do
	void $ runRedisChecked db $ Redis.sadd (redisKey key) (map encodeUtf8 new)
	existing <- (fromMaybe [] . (readZ =<<)) <$>
		TC.runTCM (TC.get (tcdb db) $ tcKey key)
	True <- TC.runTCM $
		TC.put (tcdb db) (tcKey key) (show $ nub $ (map textToString new) ++ existing)
	return ()

srem :: (HasCallStack) => DB -> Key -> [Text] -> IO ()
srem db key toremove = do
	void $ runRedisChecked db $ Redis.srem (redisKey key) (map encodeUtf8 toremove)
	existing <- (fromMaybe [] . (readZ =<<)) <$>
		TC.runTCM (TC.get (tcdb db) $ tcKey key)
	True <- TC.runTCM $
		TC.put (tcdb db) (tcKey key) (show $ filter (`notElem` toremove) existing)
	return ()

smembers :: (HasCallStack) => DB -> Key -> IO [Text]
smembers db key = do
	redisResult <- map T.decodeUtf8 <$> runRedisChecked db (Redis.smembers (redisKey key))
	tcResult <- (fromMaybe [] . (readZ =<<)) <$>
		TC.runTCM (TC.get (tcdb db) $ tcKey key)
	return $ nub $ redisResult ++ tcResult

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
hset db key newitems = do
	void $ runRedisChecked db (Redis.hmset (redisKey key) (map (encodeUtf8 *** redisMaybe) newitems))
	items <- (fromMaybe [] . (readZ =<<)) <$>
		TC.runTCM (TC.get (tcdb db) $ tcKey key)
	let items' = nubBy (equating fst) (newitems ++ items)
	True <- TC.runTCM $ TC.put (tcdb db) (tcKey key) (show items')
	return ()

-- WARNING: Right now this function assumes all values are of type Maybe String
hdel :: (HasCallStack) => DB -> Key -> [Text] -> IO ()
hdel db key toremove = do
	void $ runRedisChecked db (Redis.hdel (redisKey key) (map encodeUtf8 toremove))
	items <- (fromMaybe [] . (readZ =<<)) <$>
		TC.runTCM (TC.get (tcdb db) $ tcKey key)
	let items' = filter ((`notElem` toremove) . fst) (items :: [(Text, Maybe String)])
	True <- TC.runTCM $ TC.put (tcdb db) (tcKey key) (show items')
	return ()

hgetall :: (HasCallStack) => DB -> Key -> IO [(Text, Maybe Text)]
hgetall db key = do
	redisResult <- map (T.decodeUtf8 *** readRedisMaybe) <$>
		runRedisChecked db (Redis.hgetall (redisKey key))
	tcResult <- (fromMaybe [] . (readZ =<<)) <$>
		TC.runTCM (TC.get (tcdb db) $ tcKey key)
	return $ nubBy (equating fst) (redisResult ++ tcResult)

foldKeysM :: (HasCallStack) => DB -> Key -> b -> (b -> Key -> IO b) -> IO b
foldKeysM db (Key prefix) z f = do
	keys <- map tcParseKey <$> TC.runTCM (TC.fwmkeys (tcdb db) (tcKey $ Key $ prefix ++ [""]) maxBound)
	z' <- foldM f z (keys :: [Key])
	go keys Redis.cursor0 z'
	where
	pattern = redisKey $ Key $ prefix ++ ["*"]
	go skipKeys cursor acc = do
		(cursor', keys) <- runRedisChecked db $ Redis.scanOpts cursor (Redis.ScanOpts (Just pattern) (Just 100))
		acc' <- foldM f acc $ filter (`notElem` skipKeys) $ map redisParseKey keys
		if cursor' == Redis.cursor0 then return acc' else
			go skipKeys cursor' acc'

byJid :: JID -> [String] -> Key
byJid jid subkey = Key $ (textToString $ bareTxt jid) : subkey

-- | Used when we know the JID is @cheogram.com, for example
--   So usually this is ByTel, really
byNode :: (HasCallStack) => JID -> [String] -> Key
byNode (JID { jidNode = Just node }) subkey =
	Key $ (textToString $ strNode node) : subkey
byNode jid _ = error $ "JID without node used in byNode: " ++ show jid
