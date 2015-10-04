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
import Data.Char (isPrint, ord)
import Data.Monoid ((<>))
import Data.Text.Encoding (encodeUtf8)
import GHC.TypeLits (Symbol)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import Render

-- | Alias for Lua table.
type a := b = [(a, b)]
type Proc a = () -> a
type Break = (a, b) -> Proc ()
type Return a = a -> ()

infixr 0 :*
type a :* b = (a, b)
(:*) = (,)

data Literal a where
  LNil :: Literal ()
  LBoolean :: Bool -> Literal Bool
  LNumber :: Double -> Literal Double
  LString :: ByteString -> Literal ByteString

  LDo :: Literal (Proc () -> ())
  LIf :: Literal ([(Bool, Proc ())] -> Maybe (Proc ()) -> ())
  LForN :: Literal (Int -> Int -> Int -> (Proc () -> a -> ()) -> ())
  LFor :: Literal (Proc (Maybe a) -> (Break -> a -> ()) -> ())
  LWhile :: Literal (Proc Bool -> (Break -> ()) -> ())
  LRepeat :: Literal (Proc Bool -> (Break -> ()) -> ())

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
  lit = XUnsafeCoerce . XLiteral . LString . B.fromStrict . encodeUtf8 . T.pack

data Expr a where
  XLiteral :: Literal a -> Expr a
  XTable :: [(Expr a, Expr b)] -> Expr (a := b)
  XFunction :: (Return -> Expr a -> Expr b) -> Expr (a -> b)
  XApply :: Expr (a -> b) -> Expr a -> Expr b
  XLocal :: Expr a -> (Expr a -> Expr b) -> Expr b
  XPackPair :: Expr (a, b) -> (Expr a -> Expr c) -> Expr (a :* b)
  XConstant :: Expr a -> (Expr a -> Expr b) -> Expr b
  XVariable :: ByteString -> Expr b

  XUnsafeCoerce :: Expr a -> Expr b

f :: Expr (() -> (a, b))
f = fun $ \ _ ->
  tuple2 (x :* y)

padStart :: Int -> a -> [a] -> [a]
padStart n c s = replicate (n - length s) c <> s

compile :: Expr a -> ByteString
compile = render 0 0 . compileExpr

compileExpr :: Expr a -> Render
compileExpr (XLiteral x) = compileLiteral x
compileExpr (XUnsafeCoerce x)  = compileExpr x

compileLiteral :: Literal a -> Render
compileLiteral LNil = r_token "nil"
compileLiteral (LBoolean True) = r_token "true"
compileLiteral (LBoolean False) = r_token "false"
compileLiteral (LNumber n) = r_token (B.pack (show n))
compileLiteral (LString s) = compileString s
compileLiteral _ = error "compileLiteral: internal error (bad literal)"

compileString :: ByteString -> Render
compileString s = r_token ("\"" <> B.concatMap compileChar s <> "\"")
  where compileChar c
          | not (isPrint c) = B.pack ("\\" <> padStart 3 '0' (show (ord c)))
          | c == '\\'       = "\\"
          | c == '"'        = "\""
          | otherwise       = B.singleton c

{-
constants: floated out automatically
externals: (free variables); never renamed

mutation? IO monad? --> for now just assume all functions are impure

-}

-- -- nil | a
-- ljust :: MaybeType a => Literal (a -> Maybe a)

-- -- {x, y}
-- lpair :: Expr (a -> b -> (a, b))

-- -- {i, x}
-- lleft :: Literal (a -> Either a b)
-- lright :: Literal (b -> Either a b)

-- data TypeError (s :: Symbol) a

-- type family CheckMaybeType a where
--   CheckMaybeType () = TypeError "Nil is not allowed:" ()
--   CheckMaybeType (Maybe a) = TypeError "Maybe cannot be nested:" (Maybe a)
--   CheckMaybeType a = ()

-- type family CheckNotEqual a b where
--   CheckNotEqual a a = TypeError "Types must not be equal:" a
--   CheckNotEqual a b = ()

-- type MaybeType a = CheckMaybeType a ~ ()
