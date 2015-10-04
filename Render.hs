{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Render where
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.String (IsString(fromString))
import qualified Data.ByteString.Lazy.Char8 as B
import Common
import Utils

-- |
-- @
-- (Indentation, Precedence) -> Output
-- @
newtype Render = Render ((Int, Int) -> ByteString)
               deriving Monoid

instance IsString Render where
  fromString = atom . stringToBytestring

render :: Int -> Int -> Render -> ByteString
render = curry render_

render_ :: (Int, Int) -> Render -> ByteString
render_ c (Render f) = f c

atom :: ByteString -> Render
atom s = Render (const s)

intercalate :: Render -> [Render] -> Render
intercalate r0 rs =
  Render $ \ c -> B.intercalate (render_ c r0) ((render_ c) <$> rs)
