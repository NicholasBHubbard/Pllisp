{-# LANGUAGE OverloadedStrings #-}

-- MODULE

module Pllisp.Module where

import qualified Pllisp.CST      as CST
import qualified Pllisp.SrcLoc   as Loc
import qualified Pllisp.Type     as Ty
import qualified Pllisp.TypeCheck as TC

import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import qualified Data.Text       as T
import           System.FilePath (takeBaseName, takeFileName)

-- TOP-LEVEL DESUGARING

-- | Desugar a list of top-level expressions into type declarations followed by
-- a single flat let with unit body. Type declarations pass through unchanged.
-- Top-level lets have their bindings flattened and their body bound to _.
-- Bare expressions are bound to _.
desugarTopLevel :: CST.CST -> CST.CST
desugarTopLevel exprs =
  let (types, binds) = foldr classifyExpr ([], []) exprs
  in types ++ case binds of
    [] -> []
    _  -> [mkLet binds]
  where
    classifyExpr :: CST.Expr -> ([CST.Expr], [(CST.TSymbol, CST.Expr)]) -> ([CST.Expr], [(CST.TSymbol, CST.Expr)])
    classifyExpr loc@(Loc.Located _ (CST.ExprType _ _ _)) (ts, bs) = (loc : ts, bs)
    classifyExpr (Loc.Located _ (CST.ExprLet letBinds body)) (ts, bs) =
      (ts, letBinds ++ [(wildSym, body)] ++ bs)
    classifyExpr expr (ts, bs) =
      (ts, [(wildSym, expr)] ++ bs)

    wildSym :: CST.TSymbol
    wildSym = CST.TSymbol "_" Nothing

    mkLet :: [(CST.TSymbol, CST.Expr)] -> CST.Expr
    mkLet binds = Loc.Located dummySpan (CST.ExprLet binds unitExpr)

    unitExpr :: CST.Expr
    unitExpr = Loc.Located dummySpan CST.ExprUnit

    dummySpan :: Loc.Span
    dummySpan = Loc.Span (Loc.Pos "<desugar>" 0 0) (Loc.Pos "<desugar>" 0 0)

-- EXPORT COLLECTION

-- | Collect exported symbols from a typechecked program.
-- Exports include: let binding names (excluding _) and constructor names from type decls.
collectExports :: TC.TResolvedCST -> M.Map CST.Symbol Ty.Type
collectExports = M.unions . map collectFromExpr
  where
    collectFromExpr (Loc.Located _ (Ty.Typed _ (TC.TRLet binds _))) =
      M.fromList [(n, t) | (n, t, _) <- binds, n /= "_"]
    collectFromExpr (Loc.Located _ (Ty.Typed _ (TC.TRType _ _ ctors))) =
      M.fromList [(CST.dcName dc, ctorType dc) | dc <- ctors]
    collectFromExpr _ = M.empty

    ctorType (CST.DataCon _ []) = Ty.TyUnit  -- placeholder; real type comes from context
    ctorType (CST.DataCon _ _)  = Ty.TyUnit

-- DEPENDENCY ORDERING

-- | Topological sort of module dependencies.
-- Returns Left on cycle, Right with modules in compilation order.
dependencyOrder :: M.Map CST.Symbol [CST.Symbol] -> Either String [CST.Symbol]
dependencyOrder deps = go S.empty S.empty (M.keys deps) []
  where
    go _ _ [] acc = Right (reverse acc)
    go visited visiting (m:ms) acc
      | S.member m visited  = go visited visiting ms acc
      | S.member m visiting = Left ("circular import involving " ++ T.unpack m)
      | otherwise =
          let visiting' = S.insert m visiting
              children = M.findWithDefault [] m deps
          in case goChildren visited visiting' children of
            Left err -> Left err
            Right (visited', childAcc) ->
              go (S.insert m visited') (S.delete m visiting') ms (m : childAcc ++ acc)

    goChildren visited _ [] = Right (visited, [])
    goChildren visited visiting (c:cs)
      | S.member c visited = goChildren visited visiting cs
      | S.member c visiting = Left ("circular import involving " ++ T.unpack c)
      | otherwise =
          let visiting' = S.insert c visiting
              children = M.findWithDefault [] c deps
          in case goChildren visited visiting' children of
            Left err -> Left err
            Right (visited', childAcc) ->
              case goChildren (S.insert c visited') (S.delete c visiting') cs of
                Left err -> Left err
                Right (visited'', csAcc) -> Right (visited'', c : childAcc ++ csAcc)

-- MODULE NAME VALIDATION

-- | Check that a module name matches its filename.
-- Returns Nothing on success, Just error message on mismatch.
validateModuleName :: CST.Symbol -> FilePath -> Maybe String
validateModuleName name fp =
  let baseName = T.toUpper (T.pack (takeBaseName (takeFileName fp)))
  in if name == baseName
     then Nothing
     else Just ("module name " ++ T.unpack name ++ " does not match filename " ++ fp)
