{-# LANGUAGE OverloadedStrings #-}
module LuaSyntax where
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Render as R
newtype Name = Name ByteString

data Stat
  = Stat ByteString
  | Set [Var] [Name] Exp
  | Do [Stat]
  | While Exp [Stat]
  | Repeat [Stat] Exp

    -- skip subblocks if empty
  | If [(Exp, [Stat])] [Stat]

    -- don't render step if it's "obviously 1"
  | ForN Name Exp Exp Exp [Stat]
  | For Name [Exp] [Stat]

  | Local Name [Name] (Maybe [Exp])

    -- Return and Break must be LAST statemetns
  | Return [Exp]
  | Break

    -- todo : figure out how to convert:
    -- f.x = function() ==> function f.x() do ... end
    -- local f = function() ===>  local function f()
    -- f = function() ===>  local function f()

data Var
  = VarName Name
  | VarIndex Exp Exp -- left is subject to the same prefixexp rules
  | VarMember Exp Name -- left is subject to the same prefixexp rules

data Exp
  = Exp ByteString
  | Nil
  | Bool Bool
  | Number Double
  | String ByteString
  | Ellipsis

  | Function [Name] Varargs [Stat]
  | Var Var

    -- if left is var.func and first argument is same var, then
    -- convert to var:func(...) [this rule only for funcs]

    -- if left is var, no parens
    -- if left is call, no parens
    -- otherwise, parens left
    -- if right is one table literal or string literal, no parens
    -- otherwise, parens right
  | Call Exp [Exp]

    -- makes a difference for packs
  | Wrap Exp

    -- if it's Just (Variable _) then we use x = y syntax
    -- otherwise [x] = y
  | Table [(Maybe Exp, Exp)]

  | BinOp BinOp Exp Exp
  | UnOp UnOp Exp Exp

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | Pow
  | Mod
  | Cat
  | Lt
  | Lte
  | Gt
  | Gte
  | Eq
  | Neq
  | And
  | Or

binOpInfo :: [(BinOp, (ByteString, Int)]
binOpInfo =
  [ (Add, ("+",   6))
  , (Sub, ("-",   6))
  , (Mul, ("*",   7))
  , (Div, ("/",   7))
  , (Pow, ("^",   9))
  , (Mod, ("%",   7))
  , (Cat, ("..",  5))
  , (Lt,  ("<",   4))
  , (Lte, ("<=",  4))
  , (Gt,  (">",   4))
  , (Gte, (">=",  4))
  , (Eq,  ("==",  4))
  , (Neq, ("~=",  4))
  , (And, ("and", 3))
  , (Or,  ("or",  2))
  ]

data UnOp
  = Neg
  | Not
  | Len

unOpInfo :: [(UnOp, (ByteString, Int)]
unOpInfo =
  [ (Neg, ("-",   8))
  , (Not, ("not", 8))
  , (Len, ("#",   8))

data Varargs = Varargs | NoVarargs

-- Note: packs are weird
