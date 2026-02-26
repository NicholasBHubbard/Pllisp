{-# LANGUAGE OverloadedStrings #-}

-- MODULE

module Pllisp.Typecheck where

import qualified Pllisp.CST as CST
import qualified Pllisp.Resolve as Resolve
import qualified Pllisp.SrcLoc as Loc
import qualified Pllisp.Type as Ty

-- CORE

data TypeError = TypeError
  { teSpan     :: Loc.Span
  , teMsg      :: String
  , teExpected :: Maybe Ty.Type
  , teActual   :: Maybe Ty.Type
  } deriving (Eq, Show)

type TyExpr = Loc.Located (Ty.Typed TyRExprF)
type TypedResolvedCST = [TyExpr]

data TyRExprF
  = TyLit  CST.Literal
  | TyBool Bool
  | TyVar  Resolve.VarBinding
  | TyLam  [CST.TSymbol] (Maybe Ty.Type) TyExpr
  | TyLet  [(CST.TSymbol, TyExpr)] TyExpr
  | TyIf   TyExpr TyExpr TyExpr
  | TyApp  Resolve.VarBinding [TyExpr]
  deriving (Eq, Show)

typecheck :: Resolve.ResolvedCST -> Either [TypeError] TypedResolvedCST
typecheck = traverse (typecheckExpr [])

typecheckExpr :: Resolve.ResolveScope -> Resolve.RExpr -> Either [TypeError] TyExpr
typecheckExpr sc (Loc.Located sp expr) = Loc.Located sp <$> case expr of
  Resolve.RLit l -> Right (typecheckLit l)
  Resolve.RBool b -> Right (typecheckBool b)
  Resolve.RVar v -> typecheckVar sc v
  Resolve.RLam args rtype body -> typecheckLam sc args rtype body
  Resolve.RLet binds body -> typecheckLet sc binds body
  Resolve.RIf c t e -> typecheckIf sc c t e
  Resolve.RApp v args -> typecheckApp sc v args

typecheckLit :: CST.Literal -> Ty.Typed TyRExprF
typecheckLit (CST.LitInt l) = Ty.Typed Ty.TyInt (TyLit (CST.LitInt l))
typecheckLit (CST.LitFlt f) = Ty.Typed Ty.TyFlt (TyLit (CST.LitFlt f))
typecheckLit (CST.LitStr s) = Ty.Typed Ty.TyStr (TyLit (CST.LitStr s))

typecheckBool :: Bool -> Ty.Typed TyRExprF
typecheckBool b = Ty.Typed Ty.TyBool (TyBool b)

typecheckVar ::
     Resolve.ResolveScope
  -> Resolve.VarBinding
  -> Either [TypeError] (Ty.Typed TyRExprF)
typecheckVar = undefined

typecheckLam ::
     Resolve.ResolveScope
  -> [CST.TSymbol]
  -> Maybe Ty.Type
  -> Resolve.RExpr
  -> Either [TypeError] (Ty.Typed TyRExprF)
typecheckLam sc args rtype body = undefined

typecheckLet ::
     Resolve.ResolveScope
  -> [(CST.TSymbol, Resolve.RExpr)]
  -> Resolve.RExpr
  -> Either [TypeError] (Ty.Typed TyRExprF)
typecheckLet sc binds body = undefined

typecheckIf ::
     Resolve.ResolveScope
  -> Resolve.RExpr
  -> Resolve.RExpr
  -> Resolve.RExpr
  -> Either [TypeError] (Ty.Typed TyRExprF)
typecheckIf sc c t e = do
  c' <- typecheckExpr sc c
  t' <- typecheckExpr sc t
  e' <- typecheckExpr sc e
  pure $ undefined

typecheckApp ::
     Resolve.ResolveScope
  -> Resolve.VarBinding
  -> [Resolve.RExpr]
  -> Either [TypeError] (Ty.Typed TyRExprF)
typecheckApp = undefined

-- HELPERS

errTypeMismatch :: Ty.Type -> Ty.Type -> Loc.Span -> TypeError
errTypeMismatch exp act sp =
  TypeError sp "type mismatch" (Just exp) (Just act)

errNotAFunction :: Ty.Type -> Loc.Span -> TypeError
errNotAFunction t sp =
  TypeError sp "attempted to call a non-function" Nothing (Just t)

errUnknownSymbolType :: String -> Loc.Span -> TypeError
errUnknownSymbolType name sp =
  TypeError sp ("unknown symbol type: " <> name) Nothing Nothing
