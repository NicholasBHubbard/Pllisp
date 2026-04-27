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
  | LitRx T.Text T.Text  -- pattern, flags
  | LitUSym T.Text
  deriving (Eq, Show)

data TSymbol = TSymbol {
  symName :: Symbol,
  symType :: Maybe Ty.Type
} deriving (Eq, Show)

type Expr = Loc.Located ExprF

type CST = [Expr]

data ExprF
  = ExprLam LamList (Maybe Ty.Type) Expr
  | ExprApp Expr [Expr]
  | ExprLet [(TSymbol, Expr)] Expr
  | ExprIf Expr Expr Expr
  | ExprSym Symbol
  | ExprBool Bool
  | ExprUnit
  | ExprLit Literal
  | ExprType Symbol [Symbol] [DataCon]  -- (TYPE Name (params...) (Ctor args...)...)
  | ExprCase Expr [(Pattern, Expr)]     -- (CASE scrutinee (pat body)...)
  | ExprFieldAccess Symbol Expr         -- (.field expr)
  | ExprKeyArg Symbol Expr              -- &key name value (only in app args)
  | ExprCls Symbol [Symbol] [Symbol] [ClassMethod]  -- (CLS Name (tyvars...) [REQUIRES (supers...)] methods...)
  | ExprInst Symbol Ty.Type [(Symbol, Expr)]  -- (INST ClassName %Type methods...)
  | ExprFFI Symbol [Ty.CType] Ty.CType  -- (FFI name (c-param-types...) c-return-type)
  | ExprFFIStruct Symbol [(Symbol, Ty.CType)]  -- (FFI-STRUCT Name (field %CType)...)
  | ExprFFIVar Symbol [Ty.CType] Ty.CType  -- (FFI-VAR name (fixed-params...) ret)
  | ExprFFIEnum Symbol [(Symbol, Integer)]  -- (FFI-ENUM Name (Variant value)...)
  | ExprFFICallback Symbol [Ty.CType] Ty.CType  -- (FFI-CALLBACK name (c-param-types...) c-return-type)
  deriving (Eq, Show)

data ClassMethod = ClassMethod
  { cmName   :: Symbol
  , cmArgTys :: [Ty.Type]
  , cmRetTy  :: Ty.Type
  } deriving (Eq, Show)

data LamList = LamList
  { llRequired :: [TSymbol]
  , llExtra    :: LamExtra
  } deriving (Eq, Show)

data LamExtra
  = NoExtra
  | OptParams [(TSymbol, Expr)]  -- %opt (param default)...
  | RestParam TSymbol            -- &rest param
  | KeyParams [(TSymbol, Expr)]  -- &key (param default)...
  deriving (Eq, Show)

data Pattern
  = PatLit  Literal
  | PatBool Bool
  | PatVar  Symbol        -- variable capture
  | PatWild               -- _ wildcard
  | PatCon  Symbol [Pattern]  -- (CtorName subpat1 subpat2...)
  deriving (Eq, Show)

data DataCon = DataCon
  { dcName   :: Symbol
  , dcArgs   :: [Ty.Type]
  , dcFields :: Maybe [Symbol]  -- Nothing = positional, Just = record
  } deriving (Eq, Show)

-- PROGRAM STRUCTURE

data Program = Program
  { progName    :: Maybe Symbol
  , progImports :: [Import]
  , progExprs   :: [Expr]
  } deriving (Eq, Show)

data Import = Import
  { impModule :: Symbol
  , impAlias  :: Symbol    -- qualifier prefix (defaults to impModule)
  , impUnqual :: [Symbol]
  } deriving (Eq, Show)
