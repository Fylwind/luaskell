{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}
module Luaskell where
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Char (isPrint, ord)
import Data.Monoid ((<>))
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T

type a := b = [(a, b)]

data Literal a where
  LNil :: Literal ()
  LBoolean :: Bool -> Literal Bool
  LNumber :: Double -> Literal Double
  LString :: ByteString -> Literal ByteString

class IsLiteral a where
  lit :: a -> Expr a

instance IsLiteral () where
  lit () = XLiteral LNil

instance IsLiteral Bool where
  lit = XLiteral . LBoolean

instance IsLiteral Double where
  lit = XLiteral . LNumber

instance IsLiteral ByteString where
  lit = XLiteral . LString

instance IsLiteral String where
  lit = XCoerce . XLiteral . LString . B.fromStrict . encodeUtf8 . T.pack

data Expr a where
  XLiteral :: Literal a -> Expr a
  XTable :: [(Expr a, Expr b)] -> Expr (a := b)
  XFunction :: (Expr a -> Expr b) -> Expr (a -> b)
  XApply :: Expr a -> (Expr a -> Expr b) -> Expr b
  XLet :: Expr a -> (Expr a -> Expr b) -> Expr b
  XCoerce :: Expr a -> Expr b

type Lua a = Expr a

compileString :: ByteString -> ByteString
compileString s = B.concatMap compileChar s
  where compileChar c
          | not (isPrint c) = B.pack ("\\" <> padBegin 3 '0' (show (ord c)))
          | c == '\\'       = "\\"
          | c == '"'        = "\""
          | otherwise       = B.singleton c

padBegin :: Int -> a -> [a] -> [a]
padBegin n c s = replicate (n - length s) c <> s

compileLiteral :: Literal a -> ByteString
compileLiteral LNil = "nil"
compileLiteral (LString s) = compileString s

compile :: Lua a -> ByteString
compile (XLiteral x) = compileLiteral x
