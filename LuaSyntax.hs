{-# LANGUAGE OverloadedStrings #-}
module LuaSyntax where
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Char (isPrint, ord)
import Data.Map (Map)
import Render (Indentation, Precedence, Render)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Render as R
import qualified Data.Map.Strict as Map
import Common
import Utils

data Stat
  = Stat ByteString
  | Set [Var] Exp
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

data Exp
  = Exp ByteString
  | Nil
  | Bool Bool
  | Number Double
  | String ByteString
  | Rest

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
  | Group Exp

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
  deriving (Eq, Ord, Read, Show)

data UnOp
  = Neg
  | Not
  | Len
  deriving (Eq, Ord, Read, Show)

data Var
  = VarName Name
  | VarIndex Exp Exp -- left is subject to the same prefixexp rules
  | VarMember Exp Name -- left is subject to the same prefixexp rules

newtype Name = Name ByteString

name_str :: Name -> ByteString
name_str (Name x) = x

data Varargs = Varargs (Maybe Name) | NoVarargs

binOpInfo :: Map BinOp (ByteString, Int)
binOpInfo =
  Map.fromList
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

unOpInfo :: Map UnOp (ByteString, Int)
unOpInfo =
  Map.fromList
  [ (Neg, ("-",   8))
  , (Not, ("not", 8))
  , (Len, ("#",   8))
  ]

lookupOpName :: Ord op => Map op (ByteString, Int) -> op -> ByteString
lookupOpName info op =
  case Map.lookup op info of
    Nothing     -> error "lookupOpName: missing operator"
    Just (x, _) -> x

renderName :: Name -> Render Precedence
renderName = R.atom . name_str

renderNames :: [Name] -> Render Precedence
renderNames = R.intercalate ", " (renderName <$> ns)

renderVar :: Var -> Render Precedence
renderVar (VarName name) = renderName name
renderVar (VarIndex e1 e2) =
  -- might be wrong (parens?)
  R.prec 10 (renderExp e1) <> "[" <> R.row (renderExp e2) <> "]"
renderVar (VarMember e n) =
  -- same here?
  R.prec 10 (renderExp e) <> "." <> renderName n

renderExp :: Exp -> Render Precedence
renderExp (Exp s) = R.atom s
renderExp Nil = "nil"
renderExp (Bool True) = "true"
renderExp (Bool False) = "false"
renderExp (Number x) = R.atom (stringToBytestring (show x))
renderExp (String s) = renderString s
renderExp Rest = "..."
renderExp (Function ns NoVarargs ss) =
  R.line ("function(" <> renderNames ns <> ")") <>
  renderStats ss <>
  R.line "end"

renderString :: ByteString -> Render Precedence
renderString s = R.atom ("\"" <> B.concatMap compileChar s <> "\"")
  where compileChar c
          | not (isPrint c) = B.pack ("\\" <> padStart 3 '0' (show (ord c)))
          | c == '\\'       = "\\"
          | c == '"'        = "\""
          | otherwise       = B.singleton c

renderStats :: [Stat] -> Render Indentation
renderStats ss = R.line . renderStat <$> ss

renderStat :: Stat -> Render Indentation
renderStat (Stat s) = R.atom s
renderStat (Set vars e) =
  R.row (R.intercalate ", " (renderVar <$> vars)) <>
  " = " <> R.row (renderExp e)

-- Note: packs are weird
