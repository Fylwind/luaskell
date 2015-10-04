module Utils where
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import Common

padStart :: Int -> a -> [a] -> [a]
padStart n c s = replicate (n - length s) c <> s

stringToBytestring :: String -> ByteString
stringToBytestring = B.fromStrict . encodeUtf8 . T.pack
