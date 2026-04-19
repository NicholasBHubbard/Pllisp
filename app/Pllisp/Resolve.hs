{-# LANGUAGE OverloadedStrings #-}

-- MODULE

module Pllisp.Resolve where

import qualified Pllisp.BuiltIn as BuiltIn
import qualified Pllisp.CST as CST
import qualified Pllisp.SrcLoc as Loc
import qualified Pllisp.Type as Ty

import qualified Control.Monad.RWS as RWS
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- CORE

type ResolvedCST = [RExpr]

type RExpr = Loc.Located RExprF

type ResolveScope = [S.Set CST.Symbol] -- head = innermost

data VarBinding = VarBinding
  { scopeIdx :: Integer
  , symName :: CST.Symbol
  } deriving (Eq, Ord, Show)

data RExprF
  = RLit  CST.Literal
  | RBool Bool
  | RUnit
  | RVar  VarBinding
  | RLam  [CST.TSymbol] (Maybe Ty.Type) RExpr
  | RLet  [(CST.TSymbol, RExpr)] RExpr
  | RIf   RExpr RExpr RExpr
  | RApp  RExpr [RExpr]
  | RType CST.Symbol [CST.Symbol] [CST.DataCon]
  | RCase RExpr [(RPattern, RExpr)]
  | RFieldAccess CST.Symbol RExpr
  deriving (Eq, Show)

data RPattern
  = RPatLit  CST.Literal
  | RPatBool Bool
  | RPatVar  CST.Symbol
  | RPatWild
  | RPatCon  CST.Symbol [RPattern]
  deriving (Eq, Show)

data ResolveError = ResolveError
  { errSpan :: Loc.Span
  , errMsg  :: String
  } deriving (Eq, Show)

-- | Resolve monad: Reader for (scope, normalization map), Writer for errors
type Resolve a = RWS.RWS (ResolveScope, M.Map CST.Symbol CST.Symbol) [ResolveError] () a

resolve :: S.Set CST.Symbol -> CST.CST -> Either [ResolveError] ResolvedCST
resolve importedNames = resolveWith importedNames M.empty

resolveWith :: S.Set CST.Symbol -> M.Map CST.Symbol CST.Symbol -> CST.CST -> Either [ResolveError] ResolvedCST
resolveWith importedNames normMap cst =
  let ctorNames = S.fromList (extractCtorNames cst)
      initialScope = [S.unions [BuiltIn.builtInNames, ctorNames, importedNames]]
      (result, (), errors) = RWS.runRWS (traverse resolveExpr cst) (initialScope, normMap) ()
  in if null errors
     then Right result
     else Left errors

extractCtorNames :: CST.CST -> [CST.Symbol]
extractCtorNames = concatMap go
  where
    go (Loc.Located _ (CST.ExprType _ _ ctors)) = map CST.dcName ctors
    go _ = []

resolveExpr :: CST.Expr -> Resolve RExpr
resolveExpr (Loc.Located sp expr) = Loc.Located sp <$> case expr of
  CST.ExprLit l  -> pure (RLit l)
  CST.ExprBool b -> pure (RBool b)
  CST.ExprUnit   -> pure RUnit
  CST.ExprSym sym -> do
    rvar <- resolveSym sym sp
    pure $ RVar rvar
  CST.ExprIf c t f -> RIf <$> resolveExpr c <*> resolveExpr t <*> resolveExpr f
  CST.ExprApp fun args -> do
    rfun <- resolveExpr fun
    rargs <- traverse resolveExpr args
    pure $ RApp rfun rargs
  CST.ExprLam params mRet body -> do
    dupCheck "duplicate lambda parameter" (map CST.symName params) sp
    let newScope = S.fromList (map CST.symName params)
    rbody <- RWS.local (\(sc, nm) -> (newScope : sc, nm)) (resolveExpr body)
    pure $ RLam params mRet rbody
  CST.ExprLet binds body -> do
    let symNames = map (CST.symName . fst) binds
        newScope = S.fromList (filter (/= "_") symNames)
    dupCheck "duplicate let binding" symNames sp
    rbinds <- RWS.local (\(sc, nm) -> (newScope : sc, nm)) $
      traverse (\(v, rhs) -> (\r -> (v, r)) <$> resolveExpr rhs) binds
    rbody <- RWS.local (\(sc, nm) -> (newScope : sc, nm)) (resolveExpr body)
    pure (RLet rbinds rbody)
  CST.ExprType name params ctors -> do
    dupCheck "duplicate type parameter" params sp
    dupCheck "duplicate data constructor" (map CST.dcName ctors) sp
    pure (RType name params ctors)
  CST.ExprFieldAccess field subExpr -> do
    rexpr <- resolveExpr subExpr
    pure $ RFieldAccess field rexpr
  CST.ExprCase scrutinee arms -> do
    rscrutinee <- resolveExpr scrutinee
    rarms <- traverse (resolveArm sp) arms
    pure $ RCase rscrutinee rarms
    where
      resolveArm armSp (pat, body) = do
        (rpat, boundVars) <- resolvePattern pat armSp
        dupCheck "duplicate pattern variable" boundVars armSp
        let newScope = S.fromList boundVars
        rbody <- RWS.local (\(sc, nm) -> (newScope : sc, nm)) (resolveExpr body)
        pure (rpat, rbody)

resolveSym :: CST.Symbol -> Loc.Span -> Resolve VarBinding
resolveSym sym sp = do
  (sc, normMap) <- RWS.ask
  case lookupSym 0 sc of
    Just binding ->
      -- Normalize qualified names: MATH.SQUARE → SQUARE
      let name = M.findWithDefault (symName binding) (symName binding) normMap
      in pure (binding { symName = name })
    Nothing -> do
      recordError sp ("symbol not in scope: " ++ show sym)
      pure $ VarBinding (-1) sym
  where
    lookupSym _ [] = Nothing
    lookupSym n (f:fs)
      | S.member sym f = Just $ VarBinding n sym
      | otherwise      = lookupSym (n+1) fs

recordError :: Loc.Span -> String -> Resolve ()
recordError sp msg = RWS.tell [ResolveError sp msg]

resolvePattern :: CST.Pattern -> Loc.Span -> Resolve (RPattern, [CST.Symbol])
resolvePattern pat sp = case pat of
  CST.PatLit l    -> pure (RPatLit l, [])
  CST.PatBool b   -> pure (RPatBool b, [])
  CST.PatWild     -> pure (RPatWild, [])
  CST.PatVar s    -> pure (RPatVar s, [s])
  CST.PatCon ctor subpats -> do
    _ <- resolveSym ctor sp
    (rpats, varss) <- unzip <$> traverse (\p -> resolvePattern p sp) subpats
    pure (RPatCon ctor rpats, concat varss)

-- HELPERS

dupCheck :: String -> [CST.Symbol] -> Loc.Span -> Resolve ()
dupCheck msg syms sp =
  let nonWild = filter (/= "_") syms
      syms' = S.fromList nonWild
  in if S.size syms' /= length nonWild
     then recordError sp msg
     else pure ()
