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
-- (Indentation or Precedence) -> Output
-- @
newtype Render c = Render (c -> ByteString)
                 deriving Monoid

newtype Indentation = Indentation Int

indentation_map :: (Int -> Int) -> Indentation -> Indentation
indentation_map f (Indentation x) = Indentation (f x)

newtype Precedence = Precedence Int

precedence_map :: (Int -> Int) -> Precedence -> Precedence
precedence_map f (Precedence x) = Precedence (f x)

instance IsString (Render c) where
  fromString = atom . stringToBytestring

render :: c -> Render c -> ByteString
render c (Render f) = f c

atom :: ByteString -> Render c
atom = Render . const

modifyContext :: (c -> c) -> Render c -> Render c
modifyContext f r = Render ((`render` r) . f)

setContext :: c -> Render c -> Render c
setContext c = modifyContext (const c)

row :: Render Precedence -> Render c
row = prec 0

prec :: Int -> Render Precedence -> Render c
prec p = atom . render (Precedence p)

indent :: Render Indentation -> Render Indentation
indent = modifyContext (indentation_map (+ 2))

line :: Render Indentation -> Render Indentation
line r =
  Render (\ (Indentation i) -> B.replicate (fromIntegral i) ' ') <> r <> "\n"

expose :: ((Render c -> ByteString) -> ByteString) -> Render c
expose f = Render (f . render)

intercalate :: Render c -> [Render c] -> Render c
intercalate r0 rs =
  expose $ \ rdr -> B.intercalate (rdr r0) (rdr <$> rs)
