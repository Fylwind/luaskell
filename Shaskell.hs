{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}
module Luaskell where
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Monoid ((<>))
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Render as R

data Exp
  = Var ByteString
  | Bnd Int
  | App Exp Exp
  | Lam (Scope Exp)
  | Let Exp (Scope Exp)
  deriving (Eq, Ord, Read, Show)

newtype Scope e = Scope e deriving (Eq, Ord, Read, Show)

n_prefix :: ByteString
n_prefix = "_ts_"

-- register 0 (R0) is reserved for return values
n_register :: Int -> ByteString
n_register i = n_prefix <> "r" <> B.pack (show i)

-- name of the stack array
n_stack = n_prefix <> "s"

-- generic array push
n_push = n_prefix <> "p"

-- generic array pop
-- (pops onto R0)
n_pop = n_prefix <> "q"

-- escapes a string
n_escape = n_prefix <> "e"

-- shift $n; r0=$1; # this yields the n-th element (in zero-based index)
n_tupleget :: Int -> ByteString
n_tupleget = n_prefix <> "g"

{-

instead of representing functions via their bodies, we represent them as a
closure, i.e. a tuple containing the function name of its prototype and the
environment

we lambda lift every function to top-level -- this is the prototype

the environment contains all captured arguments (if any)

to evaluate a function, we decompose the tuple representing the function into
its parts and then call 'func_name ${env_args[@]} $@'

-}
