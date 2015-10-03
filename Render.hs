{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Render where
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B

-- |
-- @
-- Indentation -> Precedence -> Output
-- @
newtype Render = Render (Int -> Int -> ByteString)
               deriving Monoid

render :: Int -> Int -> Render -> ByteString
render i d (Render f) = f i d

r_token :: ByteString -> Render
r_token s = Render $ \ _ _ -> s
