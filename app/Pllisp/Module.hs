{-# LANGUAGE OverloadedStrings #-}

-- MODULE

module Pllisp.Module where

import qualified Pllisp.CST      as CST
import qualified Pllisp.Resolve  as Res
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
    declInfo (CST.ExprFFI name _ _ _) = Just ("ffi", name)
    declInfo (CST.ExprFFIStruct name _) = Just ("ffi", name)
    declInfo (CST.ExprFFIVar name _ _ _) = Just ("ffi", name)
    declInfo (CST.ExprFFIEnum name _) = Just ("ffi", name)
    declInfo (CST.ExprFFICallback name _ _) = Just ("ffi", name)
    declInfo _                        = Nothing

validateProgramNames :: S.Set CST.Symbol -> CST.CST -> Either String ()
validateProgramNames protected exprs = do
  mapM_ validateReservedExpr exprs
  mapM_ validateProtectedTopLevelName (concatMap topLevelDefinedNames exprs)
  where
    validateProtectedTopLevelName name
      | name `S.member` protected = Left ("cannot redefine PRELUDE symbol: " ++ T.unpack name)
      | otherwise = Right ()

    topLevelDefinedNames (Loc.Located _ expr) = case expr of
      CST.ExprLet binds _ -> [name | (CST.TSymbol name _, _) <- binds, name /= "_"]
      CST.ExprType _ _ ctors -> map CST.dcName ctors
      CST.ExprCls _ _ _ methods -> map CST.cmName methods
      _ -> []

validateReservedExpr :: CST.Expr -> Either String ()
validateReservedExpr (Loc.Located _ expr) = case expr of
  CST.ExprLam lamList _ body -> do
    validateLamList lamList
    validateReservedExpr body
  CST.ExprApp fun args -> do
    validateReservedExpr fun
    mapM_ validateReservedExpr args
  CST.ExprLet binds body -> do
    mapM_ (validateBindingName . fst) binds
    mapM_ (validateReservedExpr . snd) binds
    validateReservedExpr body
  CST.ExprIf cond then' else' -> do
    validateReservedExpr cond
    validateReservedExpr then'
    validateReservedExpr else'
  CST.ExprCase scrutinee arms -> do
    validateReservedExpr scrutinee
    mapM_ validateArm arms
  CST.ExprFieldAccess _ arg ->
    validateReservedExpr arg
  CST.ExprKeyArg _ arg ->
    validateReservedExpr arg
  CST.ExprInst _ _ methods ->
    mapM_ (validateReservedExpr . snd) methods
  _ -> Right ()
  where
    validateArm (pat, body) = do
      validatePattern pat
      validateReservedExpr body

    validateLamList (CST.LamList required extra) = do
      mapM_ validateBindingName required
      case extra of
        CST.NoExtra -> Right ()
        CST.RestParam param ->
          validateBindingName param
        CST.OptParams params -> do
          mapM_ (validateBindingName . fst) params
          mapM_ (validateReservedExpr . snd) params
        CST.KeyParams params -> do
          mapM_ (validateBindingName . fst) params
          mapM_ (validateReservedExpr . snd) params

    validatePattern pat = case pat of
      CST.PatVar name -> validateName "pattern variable" name
      CST.PatCon _ pats -> mapM_ validatePattern pats
      _ -> Right ()

    validateBindingName (CST.TSymbol name _) = validateName "binding name" name

    validateName kind name
      | name `S.member` reservedWords = Left ("reserved word cannot be used as " ++ kind ++ ": " ++ T.unpack name)
      | otherwise = Right ()

    reservedWords =
      S.fromList ["LAM", "LET", "IF", "TRUE", "FALSE", "UNIT", "TYPE", "CASE", "MODULE", "IMPORT"]

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

qualifiedSymbol :: CST.Symbol -> CST.Symbol -> CST.Symbol
qualifiedSymbol modName name = modName <> "." <> name

moduleDefinedNames :: CST.CST -> S.Set CST.Symbol
moduleDefinedNames = foldr collect S.empty
  where
    collect (Loc.Located _ expr) acc = case expr of
      CST.ExprLet binds _ ->
        acc `S.union` S.fromList [name | (CST.TSymbol name _, _) <- binds, name /= "_"]
      CST.ExprType name _ ctors ->
        acc `S.union` S.fromList (map CST.dcName ctors)
      CST.ExprCls name _ supers methods ->
        acc `S.union` S.fromList (map CST.cmName methods)
      CST.ExprFFI name _ _ _ ->
        S.insert name acc
      CST.ExprFFIStruct name _ ->
        acc
      CST.ExprFFIVar name _ _ _ ->
        S.insert name acc
      CST.ExprFFIEnum name variants ->
        acc `S.union` S.fromList (map fst variants)
      CST.ExprFFICallback name _ _ ->
        S.insert name acc
      _ ->
        acc

moduleRenameMap :: CST.Symbol -> CST.CST -> M.Map CST.Symbol CST.Symbol
moduleRenameMap modName exprs =
  M.fromList
    [ (name, qualifiedSymbol modName name)
    | name <- S.toList (moduleDefinedNames exprs)
    ]

renameTypeSymbols :: M.Map CST.Symbol CST.Symbol -> Ty.Type -> Ty.Type
renameTypeSymbols ren ty = case ty of
  Ty.TyFun args retTy ->
    Ty.TyFun (map (renameTypeSymbols ren) args) (renameTypeSymbols ren retTy)
  Ty.TyCon name args ->
    Ty.TyCon (renameSymbol name) (map (renameTypeSymbols ren) args)
  Ty.TyApp f a ->
    Ty.TyApp (renameTypeSymbols ren f) (renameTypeSymbols ren a)
  _ ->
    ty
  where
    renameSymbol name = M.findWithDefault name name ren

renameCTypeSymbols :: M.Map CST.Symbol CST.Symbol -> Ty.CType -> Ty.CType
renameCTypeSymbols ren cty = case cty of
  Ty.CArr n inner ->
    Ty.CArr n (renameCTypeSymbols ren inner)
  Ty.CStruct name ->
    Ty.CStruct (M.findWithDefault name name ren)
  _ ->
    cty

renameSchemeSymbols :: M.Map CST.Symbol CST.Symbol -> TC.Scheme -> TC.Scheme
renameSchemeSymbols ren (TC.Forall vars ty) =
  TC.Forall vars (renameTypeSymbols ren ty)

renameTypedModuleSymbols
  :: M.Map CST.Symbol CST.Symbol
  -> TC.TResolvedCST
  -> TC.TResolvedCST
renameTypedModuleSymbols ren = map (renameTopExpr S.empty)
  where
    renameTopExpr shadow = renameExpr True shadow

    renameExpr isTop shadow (Loc.Located sp (Ty.Typed ty exprF)) =
      Loc.Located sp (Ty.Typed (renameTypeSymbols ren ty) (renameExprF isTop shadow exprF))

    renameExprF isTop shadow exprF = case exprF of
      TC.TRLit _ ->
        exprF
      TC.TRBool _ ->
        exprF
      TC.TRUnit ->
        exprF
      TC.TRVar vb ->
        TC.TRVar
          (vb
            { Res.symName =
                if S.member (Res.symName vb) shadow
                  then Res.symName vb
                  else M.findWithDefault (Res.symName vb) (Res.symName vb) ren
            })
      TC.TRLam params retTy body ->
        let paramNames = S.fromList (map fst params)
            shadow' = shadow `S.union` paramNames
        in TC.TRLam
             [(name, renameTypeSymbols ren paramTy) | (name, paramTy) <- params]
             (renameTypeSymbols ren retTy)
             (renameExpr False shadow' body)
      TC.TRLet binds body
        | isTop ->
            TC.TRLet
              [ ( renameTopBinder name
                , renameTypeSymbols ren bindTy
                , renameExpr False shadow bindExpr
                )
              | (name, bindTy, bindExpr) <- binds
              ]
              (renameExpr True shadow body)
        | otherwise ->
            let boundNames = S.fromList [name | (name, _, _) <- binds]
                shadow' = shadow `S.union` boundNames
            in TC.TRLet
                 [ ( name
                   , renameTypeSymbols ren bindTy
                   , renameExpr False shadow' bindExpr
                   )
                 | (name, bindTy, bindExpr) <- binds
                 ]
                 (renameExpr False shadow' body)
      TC.TRIf cond thenBr elseBr ->
        TC.TRIf
          (renameExpr False shadow cond)
          (renameExpr False shadow thenBr)
          (renameExpr False shadow elseBr)
      TC.TRApp fn args ->
        TC.TRApp
          (renameExpr False shadow fn)
          (map (renameExpr False shadow) args)
      TC.TRType name params ctors ->
        TC.TRType
          (renameTopBinder name)
          params
          [ ctor
              { CST.dcName = renameTopBinder (CST.dcName ctor)
              , CST.dcArgs = map (renameTypeSymbols ren) (CST.dcArgs ctor)
              }
          | ctor <- ctors
          ]
      TC.TRCase scrutinee arms ->
        TC.TRCase
          (renameExpr False shadow scrutinee)
          (map (renameArm shadow) arms)
      TC.TRLoop params body ->
        let paramNames = S.fromList (map fst params)
            shadow' = shadow `S.union` paramNames
        in TC.TRLoop
             [(name, renameTypeSymbols ren paramTy) | (name, paramTy) <- params]
             (renameExpr False shadow' body)
      TC.TRRecur args ->
        TC.TRRecur (map (renameExpr False shadow) args)
      TC.TRFFI name linkName paramTys retTy ->
        TC.TRFFI
          (renameTopBinder name)
          linkName
          (map (renameCTypeSymbols ren) paramTys)
          (renameCTypeSymbols ren retTy)
      TC.TRFFIStruct name fields ->
        TC.TRFFIStruct
          (renameTopBinder name)
          [(field, renameCTypeSymbols ren fieldTy) | (field, fieldTy) <- fields]
      TC.TRFFIVar name linkName paramTys retTy ->
        TC.TRFFIVar
          (renameTopBinder name)
          linkName
          (map (renameCTypeSymbols ren) paramTys)
          (renameCTypeSymbols ren retTy)
      TC.TRFFIEnum name variants ->
        TC.TRFFIEnum
          (renameTopBinder name)
          [(renameTopBinder variantName, value) | (variantName, value) <- variants]
      TC.TRFFICallback name paramTys retTy ->
        TC.TRFFICallback
          (renameTopBinder name)
          (map (renameCTypeSymbols ren) paramTys)
          (renameCTypeSymbols ren retTy)

    renameArm shadow (pat, body) =
      let (pat', boundNames) = renamePattern pat
      in (pat', renameExpr False (shadow `S.union` boundNames) body)

    renamePattern pat = case pat of
      TC.TRPatLit _ ->
        (pat, S.empty)
      TC.TRPatBool _ ->
        (pat, S.empty)
      TC.TRPatVar name patTy ->
        (TC.TRPatVar name (renameTypeSymbols ren patTy), S.singleton name)
      TC.TRPatWild patTy ->
        (TC.TRPatWild (renameTypeSymbols ren patTy), S.empty)
      TC.TRPatCon name patTy subPats ->
        let renamedSubs = map renamePattern subPats
        in ( TC.TRPatCon
               (renameTopBinder name)
               (renameTypeSymbols ren patTy)
               (map fst renamedSubs)
           , S.unions (map snd renamedSubs)
           )

    renameTopBinder name = M.findWithDefault name name ren

renameTCEnvsSymbols
  :: M.Map CST.Symbol CST.Symbol
  -> TC.TCEnvs
  -> TC.TCEnvs
renameTCEnvsSymbols ren envs =
  TC.TCEnvs
    { TC.tceClassEnv =
        M.fromList
          [ (renameSymbol className, renameClassInfo info)
          | (className, info) <- M.toList (TC.tceClassEnv envs)
          ]
    , TC.tceMethodEnv =
        M.fromList
          [ (renameSymbol methodName, renameMethodInfo info)
          | (methodName, info) <- M.toList (TC.tceMethodEnv envs)
          ]
    , TC.tceInstanceEnv =
        M.fromList
          [ (renameSymbol className, map renameInstanceInfo instances)
          | (className, instances) <- M.toList (TC.tceInstanceEnv envs)
          ]
    }
  where
    renameSymbol name = M.findWithDefault name name ren

    renameClassInfo info =
      info
        { TC.ciSupers = map renameSymbol (TC.ciSupers info)
        }

    renameMethodInfo info =
      info
        { TC.miClass = renameSymbol (TC.miClass info)
        , TC.miArgTys = map (renameTypeSymbols ren) (TC.miArgTys info)
        , TC.miRetTy = renameTypeSymbols ren (TC.miRetTy info)
        }

    renameInstanceInfo info =
      info
        { TC.iiType = renameTypeSymbols ren (TC.iiType info)
        , TC.iiMethods =
            M.fromList
              [ (renameSymbol methodName, renameInstanceExpr body)
              | (methodName, body) <- M.toList (TC.iiMethods info)
              ]
        }

    renameInstanceExpr = renameExpr False S.empty

    renameExpr isTop shadow (Loc.Located sp (Ty.Typed ty exprF)) =
      Loc.Located sp (Ty.Typed (renameTypeSymbols ren ty) (renameExprF isTop shadow exprF))

    renameExprF isTop shadow exprF = case exprF of
      TC.TRLit _ ->
        exprF
      TC.TRBool _ ->
        exprF
      TC.TRUnit ->
        exprF
      TC.TRVar vb ->
        TC.TRVar
          (vb
            { Res.symName =
                if S.member (Res.symName vb) shadow
                  then Res.symName vb
                  else renameSymbol (Res.symName vb)
            })
      TC.TRLam params retTy body ->
        let paramNames = S.fromList (map fst params)
            shadow' = shadow `S.union` paramNames
        in TC.TRLam
             [(name, renameTypeSymbols ren paramTy) | (name, paramTy) <- params]
             (renameTypeSymbols ren retTy)
             (renameExpr False shadow' body)
      TC.TRLet binds body ->
        let boundNames = S.fromList [name | (name, _, _) <- binds]
            shadow' = shadow `S.union` boundNames
            renameBinder name =
              if isTop
                then renameSymbol name
                else name
        in TC.TRLet
             [ ( renameBinder name
               , renameTypeSymbols ren bindTy
               , renameExpr False shadow' bindExpr
               )
             | (name, bindTy, bindExpr) <- binds
             ]
             (renameExpr isTop shadow' body)
      TC.TRIf cond thenBr elseBr ->
        TC.TRIf
          (renameExpr False shadow cond)
          (renameExpr False shadow thenBr)
          (renameExpr False shadow elseBr)
      TC.TRApp fn args ->
        TC.TRApp
          (renameExpr False shadow fn)
          (map (renameExpr False shadow) args)
      TC.TRType name params ctors ->
        TC.TRType
          (renameSymbol name)
          params
          [ ctor
              { CST.dcName = renameSymbol (CST.dcName ctor)
              , CST.dcArgs = map (renameTypeSymbols ren) (CST.dcArgs ctor)
              }
          | ctor <- ctors
          ]
      TC.TRCase scrutinee arms ->
        TC.TRCase
          (renameExpr False shadow scrutinee)
          (map (renameArm shadow) arms)
      TC.TRLoop params body ->
        let paramNames = S.fromList (map fst params)
            shadow' = shadow `S.union` paramNames
        in TC.TRLoop
             [(name, renameTypeSymbols ren paramTy) | (name, paramTy) <- params]
             (renameExpr False shadow' body)
      TC.TRRecur args ->
        TC.TRRecur (map (renameExpr False shadow) args)
      TC.TRFFI name linkName paramTys retTy ->
        TC.TRFFI
          (renameSymbol name)
          linkName
          (map (renameCTypeSymbols ren) paramTys)
          (renameCTypeSymbols ren retTy)
      TC.TRFFIStruct name fields ->
        TC.TRFFIStruct
          (renameSymbol name)
          [(field, renameCTypeSymbols ren fieldTy) | (field, fieldTy) <- fields]
      TC.TRFFIVar name linkName paramTys retTy ->
        TC.TRFFIVar
          (renameSymbol name)
          linkName
          (map (renameCTypeSymbols ren) paramTys)
          (renameCTypeSymbols ren retTy)
      TC.TRFFIEnum name variants ->
        TC.TRFFIEnum
          (renameSymbol name)
          [(renameSymbol variantName, value) | (variantName, value) <- variants]
      TC.TRFFICallback name paramTys retTy ->
        TC.TRFFICallback
          (renameSymbol name)
          (map (renameCTypeSymbols ren) paramTys)
          (renameCTypeSymbols ren retTy)

    renameArm shadow (pat, body) =
      let (pat', boundNames) = renamePattern pat
      in (pat', renameExpr False (shadow `S.union` boundNames) body)

    renamePattern pat = case pat of
      TC.TRPatLit _ ->
        (pat, S.empty)
      TC.TRPatBool _ ->
        (pat, S.empty)
      TC.TRPatVar name patTy ->
        (TC.TRPatVar name (renameTypeSymbols ren patTy), S.singleton name)
      TC.TRPatWild patTy ->
        (TC.TRPatWild (renameTypeSymbols ren patTy), S.empty)
      TC.TRPatCon name patTy subPats ->
        let renamedSubs = map renamePattern subPats
        in ( TC.TRPatCon
               (renameSymbol name)
               (renameTypeSymbols ren patTy)
               (map fst renamedSubs)
           , S.unions (map snd renamedSubs)
           )

renameExportSchemes
  :: M.Map CST.Symbol CST.Symbol
  -> M.Map CST.Symbol TC.Scheme
  -> M.Map CST.Symbol TC.Scheme
renameExportSchemes ren =
  M.map (renameSchemeSymbols ren)

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

-- | Validate unqualified imports.
-- Returns Left if an import requests an unqualified name the module does not
-- export, or if the same unqualified name is imported from multiple modules.
checkImportCollisions
  :: M.Map CST.Symbol (M.Map CST.Symbol TC.Scheme)
  -> [CST.Import]
  -> Either String ()
checkImportCollisions exports imports =
  let missing = concatMap getMissing imports
      unqualPairs = concatMap getUnquals imports
      grouped = M.fromListWith (++) [(n, [m]) | (n, m) <- unqualPairs]
      collisions = M.filter (\ms -> S.size (S.fromList ms) > 1) grouped
  in if not (null missing) then Left (unlines missing)
     else if M.null collisions then Right ()
     else Left $ unlines
       [ "ambiguous import: " ++ T.unpack name ++ " is imported unqualified from "
         ++ T.unpack (T.intercalate ", " (S.toList (S.fromList mods)))
       | (name, mods) <- M.toList collisions]
  where
    getMissing (CST.Import modName _ unquals) =
      let modExports = M.findWithDefault M.empty modName exports
      in [ "module " ++ T.unpack modName ++ " does not export " ++ T.unpack n
         | n <- unquals, M.notMember n modExports
         ]
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
          exportEntries = M.toList modExports
          canonicalName name = qualifiedSymbol modName name
          qualNames = [alias <> "." <> name | (name, _) <- exportEntries]
          unqualNames = [name | (name, _) <- exportEntries, name `elem` unquals]
          canonicalCtx =
            [ (canonicalName name, scheme)
            | (name, scheme) <- exportEntries
            ]
          normMap =
            [ (alias <> "." <> name, canonicalName name)
            | (name, _) <- exportEntries
            ]
            ++
            [ (name, canonicalName name)
            | (name, _) <- exportEntries
            , name `elem` unquals
            ]
      in (qualNames ++ unqualNames, canonicalCtx, normMap)

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
                Right (visited'', csAcc) -> Right (visited'', csAcc ++ childAcc ++ [c])

-- MODULE NAME VALIDATION

-- | Check that a module name matches its filename.
-- Returns Nothing on success, Just error message on mismatch.
validateModuleName :: CST.Symbol -> FilePath -> Maybe String
validateModuleName name fp =
  let baseName = T.toUpper (T.pack (takeBaseName (takeFileName fp)))
  in if name == baseName
     then Nothing
     else Just ("module name " ++ T.unpack name ++ " does not match filename " ++ fp)
