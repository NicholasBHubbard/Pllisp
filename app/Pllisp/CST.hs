-- MODULE

module Pllisp.CST where

import qualified Data.Text as T

-- CORE

data Pos = Pos
  { posFile  :: FilePath
  , posLinum :: Int -- 1 based
  , posCol   :: Int -- 1 based
  } deriving (Eq, Show)

data Span = Span
  { spanStart :: Pos
  , spanEnd   :: Pos
  } deriving (Eq, Show)

data Located a = Located
  { locSpan :: Span
  , locVal  :: a
  } deriving (Eq, Show)

type Symbol = T.Text

data Type = Type T.Text
  deriving (Eq, Show)

data Literal =
    LitInt Integer
  | LitFlt Double
  | LitStr T.Text
  deriving (Eq, Show)

data TSymbol = TSymbol {
  symName :: Symbol,
  symType :: Maybe Type
} deriving (Eq, Show)

type Expr = Located ExprF

type CST = [Expr]

data ExprF
  = ExprLam [TSymbol] (Maybe Type) Expr
  | ExprApp Symbol [Expr]
  | ExprLet [(TSymbol, Expr)] Expr
  | ExprIf Expr Expr Expr
  | ExprSym Symbol
  | ExprBool Bool
  | ExprLit Literal
  deriving (Eq, Show)
