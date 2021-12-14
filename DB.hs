module DB (DB, Key(..), byJid, byNode, mk, get, getEnum, del, set, setEnum, sadd, srem, smembers, foldKeysM, hset, hdel, hgetall) where

import Prelude ()
import BasicPrelude

import GHC.Stack (HasCallStack)
import Control.Error (readZ)
import Network.Protocol.XMPP (JID(..), strNode)

import qualified Database.TokyoCabinet as TC
import qualified Database.Redis as Redis
import qualified Data.Text as T

import Util

data DB = DB {
	tcdb :: TC.HDB,
	redis :: Redis.Connection
}

newtype Key = Key [String]

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

tcKey :: Key -> String
tcKey (Key key) = intercalate "\0" key

tcParseKey :: String -> Key
tcParseKey str = Key $ map textToString $ T.split (=='\0') $ fromString str

get :: DB -> Key -> IO (Maybe Text)
get db key =
	fmap fromString <$> (TC.runTCM $ TC.get (tcdb db) $ tcKey key)

getEnum :: (Enum a) => DB -> Key -> IO (Maybe a)
getEnum db key =
	fmap toEnum <$> (TC.runTCM $ TC.get (tcdb db) $ tcKey key)

del :: DB -> Key -> IO ()
del db key = do
	-- May return false if key is not present, but that's fine
	void $ TC.runTCM $ TC.out (tcdb db) $ tcKey key
	return ()

set :: DB -> Key -> Text -> IO ()
set db key val = do
	True <- TC.runTCM $ TC.put (tcdb db) (tcKey key) (textToString val)
	return ()

setEnum :: (Enum a) => DB -> Key -> a -> IO ()
setEnum db key val = do
	True <- TC.runTCM $ TC.put (tcdb db) (tcKey key) (fromEnum val)
	return ()

sadd :: DB -> Key -> [Text] -> IO ()
sadd db key new = do
	existing <- (fromMaybe [] . (readZ =<<)) <$>
		TC.runTCM (TC.get (tcdb db) $ tcKey key)
	True <- TC.runTCM $
		TC.put (tcdb db) (tcKey key) (show $ nub $ (map textToString new) ++ existing)
	return ()

srem :: DB -> Key -> [Text] -> IO ()
srem db key toremove = do
	existing <- (fromMaybe [] . (readZ =<<)) <$>
		TC.runTCM (TC.get (tcdb db) $ tcKey key)
	True <- TC.runTCM $
		TC.put (tcdb db) (tcKey key) (show $ filter (`notElem` toremove) existing)
	return ()

smembers :: (Read r) => DB -> Key -> IO [r]
smembers db key =
	 (fromMaybe [] . (readZ =<<)) <$>
		TC.runTCM (TC.get (tcdb db) $ tcKey key)

hset :: (Eq k, Read k, Show k, Read v, Show v) => DB -> Key -> [(k, v)] -> IO ()
hset db key newitems = do
	items <- (fromMaybe [] . (readZ =<<)) <$>
		TC.runTCM (TC.get (tcdb db) $ tcKey key)
	let items' = nubBy (equating fst) (newitems ++ items)
	True <- TC.runTCM $ TC.put (tcdb db) (tcKey key) (show items')
	return ()

-- WARNING: Right now this function assumes all values are of type Maybe String
hdel :: DB -> Key -> [Text] -> IO ()
hdel db key toremove = do
	items <- (fromMaybe [] . (readZ =<<)) <$>
		TC.runTCM (TC.get (tcdb db) $ tcKey key)
	let items' = filter ((`notElem` toremove) . fst) (items :: [(Text, Maybe String)])
	True <- TC.runTCM $ TC.put (tcdb db) (tcKey key) (show items')
	return ()

hgetall :: (Read k, Read v) => DB -> Key -> IO [(k, v)]
hgetall db key =
	(fromMaybe [] . (readZ =<<)) <$>
		TC.runTCM (TC.get (tcdb db) $ tcKey key)

foldKeysM :: DB -> Key -> b -> (b -> Key -> IO b) -> IO b
foldKeysM db (Key prefix) z f = do
	keys <- TC.runTCM $ TC.fwmkeys (tcdb db) (tcKey $ Key $ prefix ++ [""]) maxBound
	foldM f z $ map tcParseKey (keys :: [String])

byJid :: JID -> [String] -> Key
byJid jid subkey = Key $ (textToString $ bareTxt jid) : subkey

-- | Used when we know the JID is @cheogram.com, for example
--   So usually this is ByTel, really
byNode :: (HasCallStack) => JID -> [String] -> Key
byNode (JID { jidNode = Just node }) subkey =
	Key $ (textToString $ strNode node) : subkey
byNode jid _ = error $ "JID without node used in byNode: " ++ show jid
