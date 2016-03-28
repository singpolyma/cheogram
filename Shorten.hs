import Text.Regex.PCRE (Regex)
import Text.Regex.Base.RegexLike
import qualified Data.ByteString.Char8 as C (pack, unpack)
import Network.Http.Client

polrParams :: String -> String
polrParams = (++) "http://www.polr.me/publicapi.php?action=shorten&url="

regEx :: Regex
regEx = makeRegex urlRegEx
  where 
  urlRegEx :: String
  urlRegEx = "(?:(?:(?:(?:http|https|irc)?:\\/\\/(?:(?:[!$&-.0-9;=?A-Z_a-z]|(?:\\%[a-fA-F0-9]{2}))+(?:\\:(?:[!$&-.0-9;=?A-Z_a-z]|(?:\\%[a-fA-F0-9]{2}))+)?\\@)?)?(?:(?:(?:[a-zA-Z0-9][-a-zA-Z0-9]*\\.)+(?:(?:aero|arpa|asia|a[cdefgilmnoqrstuwxz])|(?:biz|b[abdefghijmnorstvwyz])|(?:cat|com|coop|c[acdfghiklmnoruvxyz])|d[ejkmoz]|(?:edu|e[cegrstu])|f[ijkmor]|(?:gov|g[abdefghilmnpqrstuwy])|h[kmnrtu]|(?:info|int|i[delmnoqrst])|j[emop]|k[eghimnrwyz]|l[abcikrstuvy]|(?:mil|museum|m[acdeghklmnopqrstuvwxyz])|(?:name|net|n[acefgilopruz])|(?:org|om)|(?:pro|p[aefghklmnrstwy])|qa|r[eouw]|(?:space|s[abcdeghijklmnortuvyz])|(?:tel|travel|t[cdfghjklmnoprtvwz])|u[agkmsyz]|v[aceginu]|w[fs]|y[etu]|z[amw]))|(?:(?:25[0-5]|2[0-4][0-9]|[0-1][0-9]{2}|[1-9][0-9]|[1-9])\\.(?:25[0-5]|2[0-4][0-9]|[0-1][0-9]{2}|[1-9][0-9]|[0-9])\\.(?:25[0-5]|2[0-4][0-9]|[0-1][0-9]{2}|[1-9][0-9]|[0-9])\\.(?:25[0-5]|2[0-4][0-9]|[0-1][0-9]{2}|[1-9][0-9]|[0-9])))(?:\\:\\d{1,5})?)(?:\\/(?:(?:[!#&-;=?-Z_a-z~])|(?:\\%[a-fA-F0-9]{2}))*)?)(?=\\b|\\s|$)"

isLink :: String -> Bool
isLink = matchTest regEx

getShortenURL :: String -> IO String
getShortenURL url = fmap C.unpack responseBody
  where responseBody = get (C.pack (polrParams url)) concatHandler

replaceWithIO :: String -> (String -> IO String) -> IO String
replaceWithIO message shorten =
  case length message > 160 of
    True  -> unwords <$> (sequence . map replaceLink $ (words message))
             where replaceLink str 
                     | isLink str = shorten str 
                     | otherwise  = return str 
    False -> return message
