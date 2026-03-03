-- MODULE

module Pllisp.Resolve where

import qualified Pllisp.CST as CST
import qualified Pllisp.SrcLoc as Loc
import qualified Pllisp.Type as Ty

import qualified Data.Set as S

-- CORE

type ResolvedCST = [RExpr]

type RExpr = Loc.Located RExprF

type ResolveScope = [S.Set CST.Symbol] -- head = innermost

data VarBinding = VarBinding
  { scopeIdx :: Integer
  , symName :: CST.Symbol
  } deriving (Eq, Show)

data RExprF
  = RLit  CST.Literal
  | RBool Bool
  | RVar  VarBinding
  | RLam  [CST.TSymbol] (Maybe Ty.Type) RExpr
  | RLet  [(CST.TSymbol, RExpr)] RExpr
  | RIf   RExpr RExpr RExpr
  | RApp  RExpr [RExpr]
  deriving (Eq, Show)

data ResolveError = ResolveError
  { errSpan :: Loc.Span
  , errMsg  :: String
  } deriving (Eq, Show)

resolve :: CST.CST -> Either [ResolveError] ResolvedCST
resolve = traverse (resolveExpr [])

resolveExpr :: ResolveScope -> CST.Expr -> Either [ResolveError] RExpr
resolveExpr sc (Loc.Located sp expr) = Loc.Located sp <$> case expr of
  CST.ExprLit l  -> Right (RLit l)
  CST.ExprBool b -> Right (RBool b)
  CST.ExprSym sym -> do
    rvar <- resolveSym sc sym sp
    pure $ RVar rvar
  CST.ExprIf c t f -> RIf <$> resolveExpr sc c <*> resolveExpr sc t <*> resolveExpr sc f
  CST.ExprApp fun args -> do
    rfun <- resolveExpr sc fun
    rargs <- traverse (resolveExpr sc) args
    pure $ RApp rfun rargs
  CST.ExprLam params mRet body -> do
    dupCheck "duplicate lambda parameter" (map CST.symName params) sp
    rbody <- resolveExpr (S.fromList (map CST.symName params) : sc) body
    pure $ RLam params mRet rbody
  CST.ExprLet binds body -> do
    let symNames = map (CST.symName . fst) binds
    dupCheck "duplicate let binding" symNames sp
    rbinds <- traverse (\(v, rhs) -> do; rrhs <- resolveExpr sc rhs; pure (v, rrhs)) binds
    rbody <- resolveExpr (S.fromList symNames : sc) body
    pure (RLet rbinds rbody)

resolveSym :: ResolveScope -> CST.Symbol -> Loc.Span -> Either [ResolveError] VarBinding
resolveSym sc sym sp = go 0 sc
  where go _ [] = Left [ResolveError { errSpan = sp , errMsg = "symbol not in scope: " ++ show sym}]
        go n (f:fs)
          | S.member sym f = Right $ VarBinding n sym
          | otherwise      = go (n+1) fs

-- HELPERS

dupCheck :: String -> [CST.Symbol] -> Loc.Span -> Either [ResolveError] ()
dupCheck errMsg syms sp =
  let syms' = S.fromList syms
  in if S.size syms' == length syms
     then Right ()
     else Left [ResolveError sp errMsg]
