module UniquePrefix where

import Data.List
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.CaseInsensitive as CI

uniquePrefix txts = helper [] $ map (S.fromList . map CI.mk . tail . T.inits) txts

helper done (prefixes:otherPrefixes) =
	(foldl' S.difference prefixes (done ++ otherPrefixes)) : helper (prefixes:done) otherPrefixes
helper _ [] = []

--ALT: https://pastebin.com/hFKdZw2g
