{-# LANGUAGE OverloadedStrings #-}

-- MODULE

module Pllisp.Typecheck where

import qualified Pllisp.CST as CST
import qualified Pllisp.Resolve as Resolve

-- CORE

data Typed a = Typed
  { ty  :: CST.Type
  , val :: a
  } deriving (Eq, Show)

data TypeError = TypeError
  { teSpan     :: CST.Span
  , teMsg      :: String
  , teExpected :: Maybe CST.Type
  , teActual   :: Maybe CST.Type
  } deriving (Eq, Show)

type TExpr = CST.Located (Typed TRExprF)
type TypedResolvedCST = [TExpr]

data TRExprF
  = TLit  CST.Literal
  | TBool Bool
  | TVar  Resolve.VarBinding
  | TLam  [CST.TSymbol] (Maybe CST.Type) TExpr
  | TLet  [(CST.TSymbol, TExpr)] TExpr
  | TIf   TExpr TExpr TExpr
  | TApp  Resolve.VarBinding [TExpr]
  deriving (Eq, Show)

typecheck :: Resolve.ResolvedCST -> Either [TypeError] TypedResolvedCST
typecheck = traverse (typecheckExpr [])

typecheckExpr :: Resolve.ResolveScope -> Resolve.RExpr -> Either [TypeError] TExpr
typecheckExpr sc (CST.Located sp expr) = CST.Located sp <$> case expr of
  Resolve.RLit l -> Right (typecheckLit l)
  Resolve.RBool b -> Right (typecheckBool b)
  Resolve.RVar v -> typecheckVar sc v
  Resolve.RLam args rtype body -> typecheckLam sc args rtype body
  Resolve.RLet binds body -> typecheckLet sc binds body
  Resolve.RIf c t e -> typecheckIf sc c t e
  Resolve.RApp v args -> typecheckApp sc v args

typecheckLit :: CST.Literal -> Typed TRExprF
typecheckLit l = toType "INT" (TLit l)

typecheckBool :: Bool -> Typed TRExprF
typecheckBool b = toType "BOOL" (TBool b)

typecheckVar ::
     Resolve.ResolveScope
  -> Resolve.VarBinding
  -> Either [TypeError] (Typed TRExprF)
typecheckVar = undefined

typecheckLam ::
     Resolve.ResolveScope
  -> [CST.TSymbol]
  -> Maybe CST.Type
  -> Resolve.RExpr
  -> Either [TypeError] (Typed TRExprF)
typecheckLam sc args rtype body = undefined

typecheckLet ::
     Resolve.ResolveScope
  -> [(CST.TSymbol, Resolve.RExpr)]
  -> Resolve.RExpr
  -> Either [TypeError] (Typed TRExprF)
typecheckLet sc binds body = undefined

typecheckIf ::
     Resolve.ResolveScope
  -> Resolve.RExpr
  -> Resolve.RExpr
  -> Resolve.RExpr
  -> Either [TypeError] (Typed TRExprF)
typecheckIf sc c t e = do
  c' <- typecheckExpr sc c
  t' <- typecheckExpr sc t
  e' <- typecheckExpr sc e
  pure $ undefined

typecheckApp ::
     Resolve.ResolveScope
  -> Resolve.VarBinding
  -> [Resolve.RExpr]
  -> Either [TypeError] (Typed TRExprF)
typecheckApp = undefined

-- HELPERS

toType :: CST.Symbol -> a -> Typed a
toType t a = Typed (CST.Type t) a

errTypeMismatch :: CST.Type -> CST.Type -> CST.Span -> TypeError
errTypeMismatch exp act sp =
  TypeError sp "type mismatch" (Just exp) (Just act)

errNotAFunction :: CST.Type -> CST.Span -> TypeError
errNotAFunction t sp =
  TypeError sp "attempted to call a non-function" Nothing (Just t)

errUnknownSymbolType :: String -> CST.Span -> TypeError
errUnknownSymbolType name sp =
  TypeError sp ("unknown symbol type: " <> name) Nothing Nothing
