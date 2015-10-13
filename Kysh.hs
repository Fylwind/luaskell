{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}
module Kysh where
import Data.ByteString.Lazy.Char8 (ByteString)
import Text.Regex.Posix
import qualified Data.ByteString.Lazy.Char8 as B
import Common
import Render

type Str = ByteString
type Status = Int

-- [A-Za-z_][A-Za-z_0-9]* only
newtype Name = Name Str

xnameS :: String -> Name
xnameS s =
  case nameS s of
    Just x  -> x
    Nothing -> error ("xnameS: invalid name" <> s)

nameS :: String -> Maybe Name
nameS s =
  if s =~ ("^[_A-Za-z][_A-Za-z0-9]*$" :: String)
  then Just (Name (B.pack s))
  else Nothing

data Exp a where
  XStr :: Str -> Exp Str

  XVar :: Name -> Exp Str
  XArg :: Int -> Exp Str
  XNumArgs :: Exp Str
  XStatus :: Exp Str
  XArgs :: Exp [Str]

  XCat :: Exp Str -> Exp Str -> Exp Str
  XRow :: [Exp Str] -> Exp [Str]
  XCats :: [Exp [Str]] -> Exp [Str]

data Stat where
  SCall :: Exp [Str] -> Stat
  SSet :: Str -> Exp Str -> Stat
  SThen :: Stat -> Stat -> Stat
  SAnd :: Stat -> Stat -> Stat
  SOr :: Stat -> Stat -> Stat
  SFork :: Stat -> Stat -> Stat
  SPipe :: Stat -> Stat -> Stat
  SSub :: Stat -> Stat
  SFun :: Str -> Stat -> Stat
