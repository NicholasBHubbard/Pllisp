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
    classifyExpr loc@(Loc.Located _ (CST.ExprCls _ _ _)) (ts, bs) = (loc : ts, bs)
    classifyExpr loc@(Loc.Located _ (CST.ExprInst _ _ _)) (ts, bs) = (loc : ts, bs)
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
collectExports :: TC.TResolvedCST -> M.Map CST.Symbol TC.Scheme
collectExports typed =
  let letExports  = M.unions (map collectLetExports typed)
      typeDecls   = [(n, ps, cs) | Loc.Located _ (Ty.Typed _ (TC.TRType n ps cs)) <- typed]
      ctorExports = TC.buildCtorContext typeDecls
  in M.union letExports ctorExports
  where
    collectLetExports (Loc.Located _ (Ty.Typed _ (TC.TRLet binds _))) =
      M.fromList [(n, TC.generalize M.empty t) | (n, t, _) <- binds, n /= "_"]
    collectLetExports _ = M.empty

-- MERGE IMPORTED CODE

-- | Merge imported modules' typed ASTs into the local module's typed AST.
-- Type declarations from imports are prepended. Let-bindings from imports
-- are prepended into the local module's top-level let so they share scope.
mergeImportedCode :: [TC.TResolvedCST] -> TC.TResolvedCST -> TC.TResolvedCST
mergeImportedCode importedModules localTyped =
  let -- Extract type decls and let-bindings from each imported module
      (impTypes, impBinds) = mconcat (map splitTyped importedModules)
      -- Split local typed into types and lets
      (localTypes, localBinds, localBody) = splitLocal localTyped
  in impTypes ++ localTypes ++ case impBinds ++ localBinds of
    []    -> maybeBody localBody
    binds -> [mkTypedLet binds localBody]
  where
    splitTyped :: TC.TResolvedCST -> ([TC.TRExpr], [(CST.Symbol, Ty.Type, TC.TRExpr)])
    splitTyped exprs =
      let types = [e | e@(Loc.Located _ (Ty.Typed _ (TC.TRType _ _ _))) <- exprs]
          binds = concat [bs | Loc.Located _ (Ty.Typed _ (TC.TRLet bs _)) <- exprs]
      in (types, binds)

    splitLocal :: TC.TResolvedCST -> ([TC.TRExpr], [(CST.Symbol, Ty.Type, TC.TRExpr)], Maybe TC.TRExpr)
    splitLocal exprs =
      let types = [e | e@(Loc.Located _ (Ty.Typed _ (TC.TRType _ _ _))) <- exprs]
          bindsAndBody = [(bs, b) | Loc.Located _ (Ty.Typed _ (TC.TRLet bs b)) <- exprs]
      in case bindsAndBody of
        [(bs, body)] -> (types, bs, Just body)
        _            -> (types, [], Nothing)

    maybeBody Nothing  = []
    maybeBody (Just b) = [b]

    mkTypedLet binds mBody =
      let body = case mBody of
            Just b  -> b
            Nothing -> Loc.Located dummySp (Ty.Typed Ty.TyUnit TC.TRUnit)
      in Loc.Located dummySp (Ty.Typed (TC.typeOf body) (TC.TRLet binds body))

    dummySp = Loc.Span (Loc.Pos "<merge>" 0 0) (Loc.Pos "<merge>" 0 0)

-- IMPORT SCOPE BUILDING

-- | Given loaded module exports and import declarations, build:
-- 1. A set of names for the resolver (qualified + unqualified)
-- 2. A type context for the typechecker (both forms, so qualified refs typecheck)
-- 3. A normalization map (qualified → unqualified) for the resolver
buildImportScope
  :: M.Map CST.Symbol (M.Map CST.Symbol TC.Scheme)  -- module name → exports
  -> [CST.Import]
  -> (S.Set CST.Symbol, TC.Context, M.Map CST.Symbol CST.Symbol)
buildImportScope exports imports =
  let (resolveNames, tcPairs, normPairs) = mconcat (map buildOne imports)
  in (S.fromList resolveNames, M.fromList tcPairs, M.fromList normPairs)
  where
    buildOne (CST.Import modName unquals) =
      let modExports = M.findWithDefault M.empty modName exports
          -- Qualified: MODNAME.NAME → scheme
          qualNames  = [modName <> "." <> n | (n, _) <- M.toList modExports]
          qualCtx    = [(modName <> "." <> n, scheme) | (n, scheme) <- M.toList modExports]
          -- Unqualified: selectively in resolve scope, always in TC context
          unqualNames = [n | (n, _) <- M.toList modExports, n `elem` unquals]
          allUnqualCtx = M.toList modExports
          -- Normalization map: MODNAME.NAME → NAME
          normMap = [(modName <> "." <> n, n) | (n, _) <- M.toList modExports]
      in (qualNames ++ unqualNames, qualCtx ++ allUnqualCtx, normMap)

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
