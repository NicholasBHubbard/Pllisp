-- MODULE

module Pllisp.CST where

import qualified Pllisp.SrcLoc as Loc
import qualified Pllisp.Type as Ty

import qualified Data.Text as T

-- CORE

type Symbol = T.Text

data Literal =
    LitInt Integer
  | LitFlt Double
  | LitStr T.Text
  deriving (Eq, Show)

data TSymbol = TSymbol {
  symName :: Symbol,
  symType :: Maybe Ty.Type
} deriving (Eq, Show)

type Expr = Loc.Located ExprF

type CST = [Expr]

data ExprF
  = ExprLam [TSymbol] (Maybe Ty.Type) Expr
  | ExprApp Expr [Expr]
  | ExprLet [(TSymbol, Expr)] Expr
  | ExprIf Expr Expr Expr
  | ExprSym Symbol
  | ExprBool Bool
  | ExprUnit
  | ExprLit Literal
  | ExprType Symbol [Symbol] [DataCon]  -- (TYPE Name (params...) (Ctor args...)...)
  | ExprCase Expr [(Pattern, Expr)]     -- (CASE scrutinee (pat body)...)
  deriving (Eq, Show)

data Pattern
  = PatLit  Literal
  | PatBool Bool
  | PatVar  Symbol        -- variable capture
  | PatWild               -- _ wildcard
  | PatCon  Symbol [Pattern]  -- (CtorName subpat1 subpat2...)
  deriving (Eq, Show)

data DataCon = DataCon
  { dcName :: Symbol
  , dcArgs :: [Ty.Type]
  } deriving (Eq, Show)
