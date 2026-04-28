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
-- a nested let chain. Type/FFI declarations pass through unchanged.
-- Top-level lets have their bindings spliced into the chain with the body
-- continuing to subsequent expressions. Bare expressions become _ bindings.
-- This preserves proper let-polymorphism (each let group is generalized before
-- its body) while making cross-expression bindings visible.
-- Returns Left on duplicate definitions at the same nesting level.
desugarTopLevel :: CST.CST -> Either String CST.CST
desugarTopLevel exprs =
  let (decls, rest) = partition isDeclLike exprs
  in do checkDuplicateDecls decls
        checkDuplicateBindings rest
        Right $ decls ++ case rest of
          [] -> []
          _  -> [buildNestedLet rest]
  where
    isDeclLike (Loc.Located _ e) = case e of
      CST.ExprType{}        -> True
      CST.ExprCls{}         -> True
      CST.ExprInst{}        -> True
      CST.ExprFFI{}         -> True
      CST.ExprFFIStruct{}   -> True
      CST.ExprFFIVar{}      -> True
      CST.ExprFFIEnum{}     -> True
      CST.ExprFFICallback{} -> True
      _                     -> False

    buildNestedLet [] = unitExpr
    buildNestedLet [Loc.Located _ (CST.ExprLet binds body)] =
      mkLet binds (buildNestedLet [body])
    buildNestedLet [expr] =
      mkLet [(wildSym, expr)] unitExpr
    buildNestedLet (Loc.Located _ (CST.ExprLet binds body) : rest) =
      mkLet binds (buildNestedLet (body : rest))
    buildNestedLet (expr : rest) =
      mkLet [(wildSym, expr)] (buildNestedLet rest)

    wildSym = CST.TSymbol "_" Nothing
    mkLet binds body = Loc.Located dummySpan (CST.ExprLet binds body)
    unitExpr = Loc.Located dummySpan CST.ExprUnit
    dummySpan = Loc.Span (Loc.Pos "<desugar>" 0 0) (Loc.Pos "<desugar>" 0 0)

    partition _ [] = ([], [])
    partition p (x:xs) = let (ys, ns) = partition p xs
                         in if p x then (x:ys, ns) else (ys, x:ns)

-- | Check for duplicate binding names at the same nesting level.
-- Walks sequential expressions collecting names from let bindings;
-- reports the first duplicate found.
checkDuplicateBindings :: CST.CST -> Either String ()
checkDuplicateBindings = go S.empty
  where
    go _ [] = Right ()
    go seen (Loc.Located _ (CST.ExprLet binds body) : rest) = do
      seen' <- checkBinds seen binds
      -- The body is at a deeper nesting level — don't check it here.
      -- But subsequent expressions at this level continue the check.
      go seen' rest
    go seen (_ : rest) = go seen rest

    checkBinds seen [] = Right seen
    checkBinds seen ((CST.TSymbol name _, _) : bs)
      | name == "_"      = checkBinds seen bs
      | S.member name seen = Left ("duplicate top-level definition: " ++ T.unpack name)
      | otherwise        = checkBinds (S.insert name seen) bs

checkDuplicateDecls :: CST.CST -> Either String ()
checkDuplicateDecls = go S.empty
  where
    go _ [] = Right ()
    go seen (Loc.Located _ e : rest) = case declInfo e of
      Just (kind, name)
        | S.member name seen -> Left ("duplicate " ++ kind ++ " definition: " ++ T.unpack name)
        | otherwise          -> go (S.insert name seen) rest
      Nothing -> go seen rest

    declInfo (CST.ExprType name _ _)  = Just ("type", name)
    declInfo (CST.ExprCls name _ _ _) = Just ("typeclass", name)
    declInfo _                        = Nothing

-- EXPORT COLLECTION

-- | Collect exported symbols from a typechecked program.
-- Exports include: let binding names (excluding _) and constructor names from type decls.
collectExports :: TC.TCEnvs -> TC.TResolvedCST -> M.Map CST.Symbol TC.Scheme
collectExports envs typed =
  let letExports  = M.unions (map collectLetExports typed)
      typeDecls   = [(n, ps, cs) | Loc.Located _ (Ty.Typed _ (TC.TRType n ps cs)) <- typed]
      ctorExports = TC.buildCtorContext typeDecls
      methExports = TC.methodSchemes envs
  in M.unions [letExports, ctorExports, methExports]
  where
    collectLetExports (Loc.Located _ (Ty.Typed _ (TC.TRLet binds body))) =
      let named = M.fromList [(n, TC.generalize M.empty t) | (n, t, _) <- binds, n /= "_"]
          fromBody = collectLetExports body
      in M.union named fromBody
    collectLetExports _ = M.empty

-- MERGE IMPORTED CODE

-- | Merge imported modules' typed ASTs into the local module's typed AST.
-- Type declarations from imports are prepended. Let-bindings from imports
-- wrap the local module's code as an outer let, preserving the local nested
-- let chain structure.
mergeImportedCode :: [TC.TResolvedCST] -> TC.TResolvedCST -> TC.TResolvedCST
mergeImportedCode importedModules localTyped =
  let (impTypes, impBinds) = mconcat (map splitTyped importedModules)
      (localTypes, localCode) = splitLocal localTyped
  in impTypes ++ localTypes ++ case (impBinds, localCode) of
    ([], [])   -> []
    ([], code) -> code
    (bs, [])   -> [mkTypedLet bs unitExpr]
    (bs, code) -> [mkTypedLet bs (wrapCode code)]
  where
    splitTyped exprs =
      let decls = filter isDecl exprs
          binds = concatMap collectBinds exprs
      in (decls, binds)

    collectBinds :: TC.TRExpr -> [(CST.Symbol, Ty.Type, TC.TRExpr)]
    collectBinds (Loc.Located _ (Ty.Typed _ (TC.TRLet bs body))) =
      let named = [(n, t, e) | (n, t, e) <- bs, n /= "_"]
      in named ++ collectBinds body
    collectBinds _ = []

    splitLocal exprs = (filter isDecl exprs, filter (not . isDecl) exprs)

    isDecl (Loc.Located _ (Ty.Typed _ e)) = case e of
      TC.TRType{}        -> True
      TC.TRFFI{}         -> True
      TC.TRFFIStruct{}   -> True
      TC.TRFFIVar{}      -> True
      TC.TRFFIEnum{}     -> True
      TC.TRFFICallback{} -> True
      _                  -> False

    wrapCode []       = unitExpr
    wrapCode [single] = single
    wrapCode (x:xs)   = mkTypedLet [("_", TC.typeOf x, x)] (wrapCode xs)

    mkTypedLet binds body =
      Loc.Located dummySp (Ty.Typed (TC.typeOf body) (TC.TRLet binds body))

    unitExpr = Loc.Located dummySp (Ty.Typed Ty.TyUnit TC.TRUnit)
    dummySp = Loc.Span (Loc.Pos "<merge>" 0 0) (Loc.Pos "<merge>" 0 0)

-- IMPORT SCOPE BUILDING

-- | Check for unqualified name collisions across imports.
-- Returns Left with error message if the same unqualified name is imported
-- from multiple modules.
checkImportCollisions
  :: M.Map CST.Symbol (M.Map CST.Symbol TC.Scheme)
  -> [CST.Import]
  -> Either String ()
checkImportCollisions exports imports =
  let unqualPairs = concatMap getUnquals imports
      grouped = M.fromListWith (++) [(n, [m]) | (n, m) <- unqualPairs]
      collisions = M.filter (\ms -> length ms > 1) grouped
  in if M.null collisions then Right ()
     else Left $ unlines
       [ "ambiguous import: " ++ T.unpack name ++ " is imported unqualified from "
         ++ T.unpack (T.intercalate ", " mods)
       | (name, mods) <- M.toList collisions]
  where
    getUnquals (CST.Import modName _ unquals) =
      let modExports = M.findWithDefault M.empty modName exports
      in [(n, modName) | n <- unquals, M.member n modExports]

-- | Given loaded module exports and import declarations, build:
-- 1. A set of names for the resolver (qualified + unqualified)
-- 2. A type context for the typechecker (qualified + explicitly unqualified)
-- 3. A normalization map (qualified → unqualified) for the resolver
-- Qualified names use impAlias, not impModule. Only explicitly listed
-- unquals appear as unqualified names.
buildImportScope
  :: M.Map CST.Symbol (M.Map CST.Symbol TC.Scheme)  -- module name → exports
  -> [CST.Import]
  -> (S.Set CST.Symbol, TC.Context, M.Map CST.Symbol CST.Symbol)
buildImportScope exports imports =
  let (resolveNames, tcPairs, normPairs) = mconcat (map buildOne imports)
  in (S.fromList resolveNames, M.fromList tcPairs, M.fromList normPairs)
  where
    buildOne (CST.Import modName alias unquals) =
      let modExports = M.findWithDefault M.empty modName exports
          -- Qualified: ALIAS.NAME in resolve scope
          qualNames = [alias <> "." <> n | (n, _) <- M.toList modExports]
          -- Unqualified: only explicitly listed names in resolve scope
          unqualNames = [n | (n, _) <- M.toList modExports, n `elem` unquals]
          -- TC context: qualified forms + all unqualified forms (needed for
          -- normalization: resolver maps ALIAS.NAME → NAME, so TC needs NAME)
          qualCtx     = [(alias <> "." <> n, scheme) | (n, scheme) <- M.toList modExports]
          allUnqualCtx = M.toList modExports
          -- Normalization map: ALIAS.NAME → NAME
          normMap = [(alias <> "." <> n, n) | (n, _) <- M.toList modExports]
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
