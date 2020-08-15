{-
Copyright (c)2011, Falko Peters
Some modifications by Stephen Paul Weber

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

	* Redistributions of source code must retain the above copyright
	  notice, this list of conditions and the following disclaimer.

	* Redistributions in binary form must reproduce the above
	  copyright notice, this list of conditions and the following
	  disclaimer in the documentation and/or other materials provided
	  with the distribution.

	* Neither the name of Falko Peters nor the names of other
	  contributors may be used to endorse or promote products derived
	  from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}
module RedisURL (parseConnectInfo) where

import Prelude ()
import BasicPrelude
import Control.Error.Util (note)
import Control.Monad (guard)
import Data.Monoid ((<>))
import Database.Redis (ConnectInfo(..), defaultConnectInfo, PortID(..))
import Network.HTTP.Base
import Network.HTTP.Types (parseSimpleQuery)
import Network.URI (URI, parseURI, uriPath, uriScheme, uriQuery)
import Text.Read (readMaybe)

import qualified Data.ByteString.Char8 as C8

parseConnectInfo :: String -> Either String ConnectInfo
parseConnectInfo url = do
	uri <- note "Invalid URI" $ parseURI url
	case uriScheme uri of
		"redis:" -> parseRedisScheme uri
		"unix:"  -> parseUnixScheme uri
		_ -> Left "Invalid scheme"

parseUnixScheme :: URI -> Either String ConnectInfo
parseUnixScheme uri =
	return defaultConnectInfo
		{ connectHost = ""
		, connectPort = UnixSocket path
		, connectAuth = C8.pack <$> (password =<< uriAuth)
		, connectDatabase = db
		}
	where
	path = case uriPath uri of
		('/':_) -> uriPath uri
		_ -> '/' : uriPath uri
	db = fromMaybe 0 $ readMaybe . textToString . decodeUtf8 =<<
		lookup (encodeUtf8 $ fromString "db") query
	query = parseSimpleQuery (encodeUtf8 $ fromString $ uriQuery uri)
	uriAuth = parseURIAuthority $ uriToAuthorityString uri

parseRedisScheme :: URI -> Either String ConnectInfo
parseRedisScheme uri = do
	uriAuth <- note "Missing or invalid Authority"
		$ parseURIAuthority
		$ uriToAuthorityString uri

	let h = host uriAuth
	let dbNumPart = dropWhile (== '/') (uriPath uri)

	db <- if null dbNumPart
		then return $ connectDatabase defaultConnectInfo
		else note ("Invalid port: " <> dbNumPart) $ readMaybe dbNumPart

	return defaultConnectInfo
		{ connectHost = if null h
			then connectHost defaultConnectInfo
			else h
		, connectPort = maybe (connectPort defaultConnectInfo)
			(PortNumber . fromIntegral) $ port uriAuth
		, connectAuth = C8.pack <$> password uriAuth
		, connectDatabase = db
		}
