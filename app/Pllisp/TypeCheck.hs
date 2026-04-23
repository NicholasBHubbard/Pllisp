{-# LANGUAGE OverloadedStrings #-}

-- MODULE

module Pllisp.TypeCheck where

import qualified Pllisp.BuiltIn as BuiltIn
import qualified Pllisp.CST as CST
import qualified Pllisp.SrcLoc as Loc
import qualified Pllisp.Resolve as Res
import qualified Pllisp.Type as Ty

import qualified Control.Monad.RWS as RWS
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T

-- CLASS/INSTANCE ENVIRONMENTS

data TCEnvs = TCEnvs
  { tceClassEnv    :: ClassEnv
  , tceMethodEnv   :: MethodEnv
  , tceInstanceEnv :: InstanceEnv
  } deriving (Show)

emptyTCEnvs :: TCEnvs
emptyTCEnvs = TCEnvs M.empty M.empty M.empty

mergeTCEnvs :: TCEnvs -> TCEnvs -> TCEnvs
mergeTCEnvs a b = TCEnvs
  { tceClassEnv    = M.union (tceClassEnv a) (tceClassEnv b)
  , tceMethodEnv   = M.union (tceMethodEnv a) (tceMethodEnv b)
  , tceInstanceEnv = M.unionWith (++) (tceInstanceEnv a) (tceInstanceEnv b)
  }

methodSchemes :: TCEnvs -> M.Map CST.Symbol Scheme
methodSchemes envs =
  M.mapWithKey mkScheme (tceMethodEnv envs)
  where
    mkScheme _name mi =
      let className = miClass mi
          tvars = M.findWithDefault [] className (tceClassEnv envs)
          paramMap = M.fromList (zip tvars [0..])
          paramSet = S.fromList [0 .. fromIntegral (length tvars - 1)]
          resolvedArgs = map (resolveTP paramMap) (miArgTys mi)
          resolvedRet  = resolveTP paramMap (miRetTy mi)
          funTy0 = Ty.TyFun resolvedArgs resolvedRet
          freeNames = collectFreeTyCons funTy0
          nextIdx = if S.null paramSet then 0 else S.findMax paramSet + 1
          extraMap = M.fromList (zip (S.toList freeNames) [nextIdx..])
          funTy = resolveTP extraMap funTy0
          extraSet = S.fromList [nextIdx .. nextIdx + fromIntegral (S.size freeNames) - 1]
      in Forall (S.union paramSet extraSet) funTy
    resolveTP pm ty = case ty of
      Ty.TyCon name [] -> case M.lookup name pm of
        Just idx -> Ty.TyVar idx
        Nothing  -> ty
      Ty.TyCon name args -> case M.lookup name pm of
        Just idx -> foldl Ty.TyApp (Ty.TyVar idx) (map (resolveTP pm) args)
        Nothing  -> Ty.TyCon name (map (resolveTP pm) args)
      Ty.TyFun args ret -> Ty.TyFun (map (resolveTP pm) args) (resolveTP pm ret)
      Ty.TyApp f a -> Ty.TyApp (resolveTP pm f) (resolveTP pm a)
      _ -> ty

-- ENTRY POINT

typecheck :: Context -> Res.ResolvedCST -> Either [TypeError] TResolvedCST
typecheck importedCtx exprs = fmap fst (typecheckWith emptyTCEnvs importedCtx exprs)

typecheckWith :: TCEnvs -> Context -> Res.ResolvedCST -> Either [TypeError] (TResolvedCST, TCEnvs)
typecheckWith importedEnvs importedCtx exprs =
  let typeDecls = extractTypeDecls exprs
      ctorCtx = buildCtorContext typeDecls
      builtInCtx = M.map (uncurry Forall) BuiltIn.builtInSchemes
      ffiDecls = extractFFIDecls exprs
      ffiCtx = buildFFIContext ffiDecls
      ffiStructDecls = extractFFIStructDecls exprs
      ffiStructCtorCtx = buildFFIStructCtorContext ffiStructDecls
      ffiStructFieldMap = buildFFIStructFieldMap ffiStructDecls
      ffiVarDecls = extractFFIVarDecls exprs
      ffiVarCtx = buildFFIContext ffiVarDecls
      variadicNames = S.fromList [name | (name, _, _) <- ffiVarDecls]
      enumCtx = buildEnumContext exprs
      callbackCtx = buildCallbackContext exprs
      -- Build class method schemes and instance env
      classDecls = extractClassDecls exprs
      (localClassEnv, localMethodEnv, methodCtx) = buildClassContext classDecls
      classEnv = M.union localClassEnv (tceClassEnv importedEnvs)
      methodEnv = M.union localMethodEnv (tceMethodEnv importedEnvs)
      initialCtx = M.unions [importedCtx, ctorCtx, builtInCtx, ffiCtx, ffiStructCtorCtx, ffiVarCtx, enumCtx, callbackCtx, methodCtx]
      fieldMap = M.union (buildFieldMap typeDecls) ffiStructFieldMap
      -- Type-check instance method bodies in a first pass
      instDecls = extractInstDecls exprs
      localTypeNames = S.fromList [name | (name, _, _) <- typeDecls]
      importedTypeNames = collectTypeNames importedCtx
      ffiStructNames = S.fromList [name | (name, _) <- ffiStructDecls]
      knownTypeNames = S.unions [localTypeNames, importedTypeNames, ffiStructNames]
      (localInstanceEnv, instErrs) = buildInstanceEnv classEnv methodEnv initialCtx fieldMap knownTypeNames instDecls
      instanceEnv = M.unionWith (++) localInstanceEnv (tceInstanceEnv importedEnvs)
      structFieldsMap = M.fromList ffiStructDecls
      env = InferEnv initialCtx fieldMap M.empty methodEnv instanceEnv variadicNames structFieldsMap
      (typed, _, (constraints, inferErrs)) = RWS.runRWS (traverse infer exprs) env 0
      localEnvs = TCEnvs localClassEnv localMethodEnv localInstanceEnv
      fullEnvs = mergeTCEnvs localEnvs importedEnvs
  in case solveAll constraints of
    Left solveErrs -> Left (instErrs ++ inferErrs ++ solveErrs)
    Right subst
      | not (null instErrs) -> Left instErrs
      | not (null inferErrs) -> Left inferErrs
      | otherwise ->
          let resolved = apply subst typed
              missingInst = validateInstances methodEnv instanceEnv resolved
          in if not (null missingInst)
             then Left missingInst
             else Right (tcoPass (dictPass classEnv methodEnv instanceEnv resolved), fullEnvs)

-- TYPES

type TyVar = Integer

type Subst = M.Map TyVar Ty.Type

type Constraints = [Constraint]

type Context = M.Map CST.Symbol Scheme

-- | Output from inference: constraints to solve and errors encountered
type InferOutput = (Constraints, [TypeError])

-- | Field info for record desugar: (ctor name, field index, total fields)
type FieldInfo = (CST.Symbol, Int, Int)
type FieldMap = M.Map CST.Symbol FieldInfo

-- | Metadata about a function's extended params, used for call-site rewriting.
data FuncInfo = FuncInfo
  { fiRequired :: !Int
  , fiExtra    :: !FuncInfoExtra
  }

data FuncInfoExtra
  = FIPlain
  | FIOpt [Res.RExpr]                   -- default expressions (resolved, re-inferred at call site)
  | FIRest
  | FIKey [CST.Symbol] [Res.RExpr]      -- key names, default expressions

type FuncInfoMap = M.Map CST.Symbol FuncInfo

-- | Info about a typeclass method: which class, original arg types, return type.
data MethodInfo = MethodInfo
  { miClass     :: CST.Symbol
  , miArgTys    :: [Ty.Type]    -- uses TyCon "A" [] for class type vars
  , miRetTy     :: Ty.Type
  , miClassVars :: [CST.Symbol] -- class type parameter names
  } deriving (Eq, Show)

type MethodEnv = M.Map CST.Symbol MethodInfo

-- | Info about a typeclass instance: type pattern + typed method impls.
-- iiTyVars holds names that act as type variables in iiType (match anything).
data InstanceInfo = InstanceInfo
  { iiType    :: Ty.Type
  , iiTyVars  :: S.Set CST.Symbol
  , iiMethods :: M.Map CST.Symbol TRExpr
  } deriving (Show)

type ClassEnv = M.Map CST.Symbol [CST.Symbol]     -- class name -> type var names
type InstanceEnv = M.Map CST.Symbol [InstanceInfo] -- class name -> instances

-- | Full struct field info: all fields including arrays
type StructFieldMap = M.Map CST.Symbol [(CST.Symbol, Ty.CType)]

data InferEnv = InferEnv
  { ieCtx          :: !Context
  , ieFields       :: !FieldMap
  , ieFuncs        :: !FuncInfoMap
  , ieMethods      :: !MethodEnv
  , ieInstances    :: !InstanceEnv
  , ieVariadics    :: !(S.Set CST.Symbol)
  , ieStructFields :: !StructFieldMap
  }

-- | Inference monad that collects errors instead of failing
type Infer a = RWS.RWS InferEnv InferOutput TyVar a

askCtx :: Infer Context
askCtx = ieCtx <$> RWS.ask

localCtx :: (Context -> Context) -> Infer a -> Infer a
localCtx f = RWS.local (\e -> e { ieCtx = f (ieCtx e) })

localFuncs :: (FuncInfoMap -> FuncInfoMap) -> Infer a -> Infer a
localFuncs f = RWS.local (\e -> e { ieFuncs = f (ieFuncs e) })

type TResolvedCST = [TRExpr]

type TRExpr = Loc.Located (Ty.Typed TRExprF)

data Scheme = Forall (S.Set TyVar) Ty.Type
  deriving (Eq, Show)

data Constraint = Constraint Loc.Span Ty.Type Ty.Type
  deriving (Eq, Show)

data TypeError = TypeError
  { teSpan :: Loc.Span
  , teMsg  :: String
  } deriving (Eq, Show)

data TRExprF
  = TRLit  CST.Literal
  | TRBool Bool
  | TRUnit
  | TRVar  Res.VarBinding
  | TRLam  [(CST.Symbol, Ty.Type)] Ty.Type TRExpr
  | TRLet  [(CST.Symbol, Ty.Type, TRExpr)] TRExpr
  | TRIf   TRExpr TRExpr TRExpr
  | TRApp  TRExpr [TRExpr]
  | TRType CST.Symbol [CST.Symbol] [CST.DataCon]
  | TRCase TRExpr [(TRPattern, TRExpr)]
  | TRLoop [(CST.Symbol, Ty.Type)] TRExpr
  | TRRecur [TRExpr]
  | TRFFI CST.Symbol [Ty.CType] Ty.CType
  | TRFFIStruct CST.Symbol [(CST.Symbol, Ty.CType)]
  | TRFFIVar CST.Symbol [Ty.CType] Ty.CType
  | TRFFIEnum CST.Symbol [(CST.Symbol, Integer)]
  | TRFFICallback CST.Symbol [Ty.CType] Ty.CType
  deriving (Eq, Show)

data TRPattern
  = TRPatLit  CST.Literal
  | TRPatBool Bool
  | TRPatVar  CST.Symbol Ty.Type
  | TRPatWild Ty.Type
  | TRPatCon  CST.Symbol Ty.Type [TRPattern]
  deriving (Eq, Show)

-- SUBSTITUTION

class Substitutable a where
  apply :: Subst -> a -> a
  tvs :: a -> S.Set TyVar

instance Substitutable Ty.Type where
  tvs (Ty.TyVar v)     = S.singleton v
  tvs (Ty.TyFun ats t) = foldr (S.union . tvs) S.empty (t:ats)
  tvs (Ty.TyCon _ ts)  = foldr (S.union . tvs) S.empty ts
  tvs (Ty.TyApp f a)   = tvs f `S.union` tvs a
  tvs _                = S.empty

  apply s t@(Ty.TyVar tv)   = M.findWithDefault t tv s
  apply s (Ty.TyFun ats rt) = Ty.TyFun (map (apply s) ats) (apply s rt)
  apply s (Ty.TyCon sym ts) = Ty.TyCon sym $ map (apply s) ts
  apply s (Ty.TyApp f a)    = Ty.TyApp (apply s f) (apply s a)
  apply _ t                 = t

instance Substitutable Scheme where
  tvs (Forall vs t) = tvs t `S.difference` vs
  apply s (Forall vs t) = Forall vs $ apply (foldr M.delete s vs) t

instance Substitutable Constraint where
  tvs (Constraint _ t1 t2) = tvs t1 `S.union` tvs t2
  apply s (Constraint sp t1 t2) = Constraint sp (apply s t1) (apply s t2)

instance Substitutable a => Substitutable [a] where
  tvs l = foldr (S.union . tvs) S.empty l
  apply s = map (apply s)

instance Substitutable a => Substitutable (Loc.Located a) where
  tvs (Loc.Located _ a) = tvs a
  apply s (Loc.Located sp a) = Loc.Located sp (apply s a)

instance Substitutable a => Substitutable (Ty.Typed a) where
  tvs (Ty.Typed t a) = tvs t `S.union` tvs a
  apply s (Ty.Typed t a) = Ty.Typed (apply s t) (apply s a)

instance Substitutable TRPattern where
  tvs (TRPatLit _)       = S.empty
  tvs (TRPatBool _)      = S.empty
  tvs (TRPatVar _ t)     = tvs t
  tvs (TRPatWild t)      = tvs t
  tvs (TRPatCon _ t ps)  = tvs t `S.union` foldr (S.union . tvs) S.empty ps

  apply _ (TRPatLit l)      = TRPatLit l
  apply _ (TRPatBool b)     = TRPatBool b
  apply s (TRPatVar n t)    = TRPatVar n (apply s t)
  apply s (TRPatWild t)     = TRPatWild (apply s t)
  apply s (TRPatCon c t ps) = TRPatCon c (apply s t) (map (apply s) ps)

instance Substitutable TRExprF where
  tvs (TRLit _) = S.empty
  tvs (TRBool _) = S.empty
  tvs TRUnit = S.empty
  tvs (TRVar _) = S.empty
  tvs (TRLam params retTy body) = tvs (map snd params) `S.union` tvs retTy `S.union` tvs body
  tvs (TRLet binds body) = foldr S.union S.empty [tvs t `S.union` tvs e | (_, t, e) <- binds] `S.union` tvs body
  tvs (TRIf c t e) = tvs c `S.union` tvs t `S.union` tvs e
  tvs (TRApp f as) = tvs f `S.union` tvs as
  tvs (TRType _ _ _) = S.empty
  tvs (TRCase scr arms) = tvs scr `S.union` foldr S.union S.empty [tvs p `S.union` tvs e | (p, e) <- arms]
  tvs (TRLoop params body) = foldr (S.union . tvs . snd) S.empty params `S.union` tvs body
  tvs (TRRecur args) = tvs args
  tvs (TRFFI _ _ _) = S.empty
  tvs (TRFFIStruct _ _) = S.empty
  tvs (TRFFIVar _ _ _) = S.empty
  tvs (TRFFIEnum _ _) = S.empty
  tvs (TRFFICallback _ _ _) = S.empty

  apply _ (TRLit l) = TRLit l
  apply _ (TRBool b) = TRBool b
  apply _ TRUnit = TRUnit
  apply _ (TRVar v) = TRVar v
  apply s (TRLam params retTy body) = TRLam [(n, apply s t) | (n, t) <- params] (apply s retTy) (apply s body)
  apply s (TRLet binds body) = TRLet [(n, apply s t, apply s e) | (n, t, e) <- binds] (apply s body)
  apply s (TRIf c t e) = TRIf (apply s c) (apply s t) (apply s e)
  apply s (TRApp f as) = TRApp (apply s f) (apply s as)
  apply _ (TRType n ps cs) = TRType n ps cs
  apply s (TRCase scr arms) = TRCase (apply s scr) [(apply s p, apply s e) | (p, e) <- arms]
  apply s (TRLoop params body) = TRLoop [(n, apply s t) | (n, t) <- params] (apply s body)
  apply s (TRRecur args) = TRRecur (apply s args)
  apply _ (TRFFI n pts rt) = TRFFI n pts rt
  apply _ (TRFFIStruct n fs) = TRFFIStruct n fs
  apply _ (TRFFIVar n pts rt) = TRFFIVar n pts rt
  apply _ (TRFFIEnum n vs) = TRFFIEnum n vs
  apply _ (TRFFICallback n pts rt) = TRFFICallback n pts rt

compose :: Subst -> Subst -> Subst
compose a b = M.map (apply a) (b `M.union` a)

-- CONTEXT BUILDING

extractTypeDecls :: [Res.RExpr] -> [(CST.Symbol, [CST.Symbol], [CST.DataCon])]
extractTypeDecls = foldr go []
  where
    go (Loc.Located _ (Res.RType name params ctors)) acc = (name, params, ctors) : acc
    go _ acc = acc

-- | Extract type constructor names from a context by scanning constructor
-- return types (the TyCon head of function return types in schemes).
collectTypeNames :: Context -> S.Set CST.Symbol
collectTypeNames ctx = S.fromList [n | Forall _ ty <- M.elems ctx, n <- headTyCons ty]
  where
    headTyCons (Ty.TyFun _ (Ty.TyCon n _)) = [n]
    headTyCons (Ty.TyCon n _) = [n]
    headTyCons _ = []

extractFFIDecls :: [Res.RExpr] -> [(CST.Symbol, [Ty.CType], Ty.CType)]
extractFFIDecls = foldr go []
  where
    go (Loc.Located _ (Res.RFFI name paramTys retTy)) acc = (name, paramTys, retTy) : acc
    go _ acc = acc

buildFFIContext :: [(CST.Symbol, [Ty.CType], Ty.CType)] -> Context
buildFFIContext = M.fromList . map (\(name, paramCTys, retCTy) ->
  let -- CPtr parameters become polymorphic type variables so they accept
      -- any pointer type (strings, structs, closures, etc.)
      (paramTys, nextVar) = foldr (\ct (acc, n) -> case ct of
        Ty.CPtr -> (Ty.TyVar n : acc, n + 1)
        _       -> (Ty.cTypeToPllisp ct : acc, n)) ([], 100) paramCTys
      (retTy, maxVar) = case retCTy of
        Ty.CPtr -> (Ty.TyVar nextVar, nextVar + 1)
        _       -> (Ty.cTypeToPllisp retCTy, nextVar)
      ptrVars = S.fromList [100 .. maxVar - 1]
  in (name, Forall ptrVars (Ty.TyFun paramTys retTy)))

buildCtorContext :: [(CST.Symbol, [CST.Symbol], [CST.DataCon])] -> Context
buildCtorContext = M.fromList . concatMap buildCtors
  where
    buildCtors (typeName, params, ctors) =
      let paramMap = M.fromList (zip params [0..])
          paramTyVars = map Ty.TyVar [0 .. fromIntegral (length params - 1)]
          resultTy = Ty.TyCon typeName paramTyVars
          paramSet = S.fromList [0 .. fromIntegral (length params - 1)]
      in map (buildCtor paramMap paramSet resultTy) ctors

    buildCtor paramMap paramSet resultTy (CST.DataCon ctorName args _fields) =
      let argTys = map (resolveTypeParams paramMap) args
          ctorTy = if null argTys
                   then resultTy
                   else Ty.TyFun argTys resultTy
          scheme = Forall paramSet ctorTy
      in (ctorName, scheme)

    resolveTypeParams paramMap ty = case ty of
      Ty.TyCon name [] -> case M.lookup name paramMap of
        Just idx -> Ty.TyVar idx
        Nothing  -> ty
      Ty.TyCon name args -> Ty.TyCon name (map (resolveTypeParams paramMap) args)
      Ty.TyFun args ret -> Ty.TyFun (map (resolveTypeParams paramMap) args) (resolveTypeParams paramMap ret)
      Ty.TyApp f a -> Ty.TyApp (resolveTypeParams paramMap f) (resolveTypeParams paramMap a)
      _ -> ty

-- FFI STRUCT CONTEXT

extractFFIStructDecls :: [Res.RExpr] -> [(CST.Symbol, [(CST.Symbol, Ty.CType)])]
extractFFIStructDecls = foldr go []
  where
    go (Loc.Located _ (Res.RFFIStruct name fields)) acc = (name, fields) : acc
    go _ acc = acc

buildFFIStructCtorContext :: [(CST.Symbol, [(CST.Symbol, Ty.CType)])] -> Context
buildFFIStructCtorContext = M.fromList . map (\(name, fields) ->
  let scalarFields = filter (isScalarCType . snd) fields
      argTys = map (Ty.cTypeToPllisp . snd) scalarFields
      resultTy = Ty.TyCon name []
      ctorTy = if null argTys then resultTy else Ty.TyFun argTys resultTy
  in (name, Forall S.empty ctorTy))

isScalarCType :: Ty.CType -> Bool
isScalarCType (Ty.CArr _ _) = False
isScalarCType _ = True

buildFFIStructFieldMap :: [(CST.Symbol, [(CST.Symbol, Ty.CType)])] -> FieldMap
buildFFIStructFieldMap = M.fromList . concatMap go
  where
    go (ctorName, fields) =
      let n = length fields
      in [(fname, (ctorName, idx, n)) | (idx, (fname, _)) <- zip [0..] fields]

-- FFI VARIADIC CONTEXT

extractFFIVarDecls :: [Res.RExpr] -> [(CST.Symbol, [Ty.CType], Ty.CType)]
extractFFIVarDecls = foldr go []
  where
    go (Loc.Located _ (Res.RFFIVar name paramTys retTy)) acc = (name, paramTys, retTy) : acc
    go _ acc = acc

-- FFI ENUM CONTEXT

buildEnumContext :: [Res.RExpr] -> Context
buildEnumContext = M.fromList . concatMap go
  where
    go (Loc.Located _ (Res.RFFIEnum _ variants)) =
      [(name, Forall S.empty Ty.TyInt) | (name, _) <- variants]
    go _ = []

buildCallbackContext :: [Res.RExpr] -> Context
buildCallbackContext = M.fromList . concatMap go
  where
    go (Loc.Located _ (Res.RFFICallback name paramCTys retCTy)) =
      let closureTy = Ty.TyFun (map Ty.cTypeToPllisp paramCTys) (Ty.cTypeToPllisp retCTy)
      in [(name, Forall S.empty (Ty.TyFun [closureTy] Ty.TyStr))]
    go _ = []

buildFieldMap :: [(CST.Symbol, [CST.Symbol], [CST.DataCon])] -> FieldMap
buildFieldMap = M.fromList . concatMap go
  where
    go (_typeName, _params, ctors) = concatMap ctorFields ctors
    ctorFields (CST.DataCon ctorName args (Just fieldNames)) =
      [(fname, (ctorName, idx, length args))
      | (idx, fname) <- zip [0..] fieldNames]
    ctorFields _ = []

-- TYPECLASS CONTEXT BUILDING

extractClassDecls :: [Res.RExpr] -> [(CST.Symbol, [CST.Symbol], [CST.ClassMethod])]
extractClassDecls = foldr go []
  where
    go (Loc.Located _ (Res.RCls name tvars methods)) acc = (name, tvars, methods) : acc
    go _ acc = acc

extractInstDecls :: [Res.RExpr] -> [(CST.Symbol, Ty.Type, [(CST.Symbol, Res.RExpr)])]
extractInstDecls = foldr go []
  where
    go (Loc.Located _ (Res.RInst className ty methods)) acc = (className, ty, methods) : acc
    go _ acc = acc

-- | Build class environment (class -> type vars), method environment (method -> info),
-- and method context (method -> polymorphic scheme).
buildClassContext :: [(CST.Symbol, [CST.Symbol], [CST.ClassMethod])]
                  -> (ClassEnv, MethodEnv, Context)
buildClassContext decls =
  let classEnv = M.fromList [(name, tvars) | (name, tvars, _) <- decls]
      methodPairs = concatMap buildMethods decls
      methodEnv = M.fromList [(name, info) | (name, info, _) <- methodPairs]
      methodCtx = M.fromList [(name, scheme) | (name, _, scheme) <- methodPairs]
  in (classEnv, methodEnv, methodCtx)
  where
    buildMethods (className, tvars, methods) =
      let paramMap = M.fromList (zip tvars [0..])
          paramSet = S.fromList [0 .. fromIntegral (length tvars - 1)]
      in map (buildMethod className tvars paramMap paramSet) methods

    buildMethod className cvars paramMap paramSet (CST.ClassMethod mname argTys retTy) =
      let resolvedArgs = map (resolveTypeParams' paramMap) argTys
          resolvedRet  = resolveTypeParams' paramMap retTy
          funTy0 = Ty.TyFun resolvedArgs resolvedRet
          -- Resolve remaining free type names (method-local type variables like a, b)
          freeNames = collectFreeTyCons funTy0
          nextIdx = if S.null paramSet then 0 else S.findMax paramSet + 1
          extraMap = M.fromList (zip (S.toList freeNames) [nextIdx..])
          funTy = resolveTypeParams' extraMap funTy0
          extraSet = S.fromList [nextIdx .. nextIdx + fromIntegral (S.size freeNames) - 1]
          scheme = Forall (S.union paramSet extraSet) funTy
      in (mname, MethodInfo className argTys retTy cvars, scheme)

    resolveTypeParams' paramMap ty = case ty of
      Ty.TyCon name [] -> case M.lookup name paramMap of
        Just idx -> Ty.TyVar idx
        Nothing  -> ty
      Ty.TyCon name args -> case M.lookup name paramMap of
        Just idx -> foldl Ty.TyApp (Ty.TyVar idx) (map (resolveTypeParams' paramMap) args)
        Nothing  -> Ty.TyCon name (map (resolveTypeParams' paramMap) args)
      Ty.TyFun args ret -> Ty.TyFun (map (resolveTypeParams' paramMap) args) (resolveTypeParams' paramMap ret)
      Ty.TyApp f a -> Ty.TyApp (resolveTypeParams' paramMap f) (resolveTypeParams' paramMap a)
      _ -> ty

-- | Collect free TyCon names that look like type variables (not builtins).
collectFreeTyCons :: Ty.Type -> S.Set CST.Symbol
collectFreeTyCons = go
  where
    builtins = S.fromList ["INT", "FLT", "STR", "BOOL", "UNIT", "RX", "REF"]
    go (Ty.TyCon name []) | not (S.member name builtins) = S.singleton name
    go (Ty.TyCon _ args) = S.unions (map go args)
    go (Ty.TyFun args ret) = S.unions (go ret : map go args)
    go (Ty.TyApp f a) = go f `S.union` go a
    go _ = S.empty

-- | Type-check instance method bodies and build the instance environment.
buildInstanceEnv :: ClassEnv -> MethodEnv -> Context -> FieldMap
                 -> S.Set CST.Symbol  -- known type constructor names
                 -> [(CST.Symbol, Ty.Type, [(CST.Symbol, Res.RExpr)])]
                 -> (InstanceEnv, [TypeError])
buildInstanceEnv classEnv methodEnv ctx fieldMap knownTypes instDecls =
  let results = map checkInst instDecls
      instEnv = M.fromListWith (++) [(cls, [inst]) | (cls, inst, _) <- results]
      errs = concatMap (\(_, _, es) -> es) results
  in (instEnv, errs)
  where
    checkInst (className, instTy, methods) =
      let kindErrs = checkInstanceKind classEnv methodEnv className instTy
          env = InferEnv ctx fieldMap M.empty methodEnv M.empty S.empty M.empty
          (typedMethods, _, (constraints, inferErrs)) =
            RWS.runRWS (traverse (checkMethod className instTy) methods) env 0
          (solveErrs, resolvedMethods) = case solveAll constraints of
            Left es -> (es, typedMethods)
            Right subst -> ([], map (\(n, e) -> (n, apply subst e)) typedMethods)
          methodMap = M.fromList resolvedMethods
          tyVars = extractInstTyVars knownTypes instTy
          inst = InstanceInfo instTy tyVars methodMap
      in (className, inst, kindErrs ++ inferErrs ++ solveErrs)

    checkMethod _className _instTy (mname, body) = do
      typed <- infer body
      pure (mname, typed)

-- | Extract type variable names from an instance type.
-- A TyCon name [] that is NOT a known type constructor is a type variable.
extractInstTyVars :: S.Set CST.Symbol -> Ty.Type -> S.Set CST.Symbol
extractInstTyVars knownTypes = go
  where
    go (Ty.TyCon name []) | not (S.member name knownTypes) = S.singleton name
    go (Ty.TyCon _ args) = S.unions (map go args)
    go (Ty.TyFun args ret) = S.unions (go ret : map go args)
    go (Ty.TyApp f a) = go f `S.union` go a
    go _ = S.empty

-- | Check that instance types have the correct kind for the class.
-- If a class param appears applied (e.g. (f a)) in method signatures,
-- the instance type must be a type constructor, not a ground type.
checkInstanceKind :: ClassEnv -> MethodEnv -> CST.Symbol -> Ty.Type -> [TypeError]
checkInstanceKind classEnv methodEnv className instTy =
  let tvars = M.findWithDefault [] className classEnv
      methods = [mi | (_, mi) <- classMethodOrder className methodEnv]
  in concatMap (checkParam methods instTy) tvars
  where
    checkParam methods iTy paramName =
      if isHKParam paramName methods && isGroundType iTy
      then [TypeError dictSp ("kind mismatch: class " ++ T.unpack className
            ++ " expects a type constructor (kind * -> *) for param "
            ++ T.unpack paramName ++ ", but got " ++ T.unpack (Ty.renderType iTy))]
      else []

    isHKParam name mis = any (\mi -> any (appearsApplied name) (miRetTy mi : miArgTys mi)) mis

    appearsApplied name (Ty.TyCon n (_:_)) | n == name = True
    appearsApplied name (Ty.TyCon _ args) = any (appearsApplied name) args
    appearsApplied name (Ty.TyFun args ret) = any (appearsApplied name) (ret:args)
    appearsApplied _ _ = False

    isGroundType Ty.TyInt  = True
    isGroundType Ty.TyFlt  = True
    isGroundType Ty.TyStr  = True
    isGroundType Ty.TyBool = True
    isGroundType Ty.TyUnit = True
    isGroundType Ty.TyRx   = True
    isGroundType _         = False

-- DICTIONARY PASSING

data DictNeed = DictNeed
  { dnClass    :: !CST.Symbol
  , dnTyVar    :: !Ty.Type
  , dnParamIdx :: !Int
  }

type DictNeedsMap = M.Map CST.Symbol [DictNeed]
type DictParamCtx = M.Map (CST.Symbol, Integer) CST.Symbol

dictSp :: Loc.Span
dictSp = Loc.Span (Loc.Pos "<dict>" 0 0) (Loc.Pos "<dict>" 0 0)

-- | Check that all monomorphic class method calls have matching instances.
validateInstances :: MethodEnv -> InstanceEnv -> TResolvedCST -> [TypeError]
validateInstances me ie = concatMap checkExpr
  where
    checkExpr (Loc.Located sp (Ty.Typed _ node)) = case node of
      TRApp (Loc.Located _ (Ty.Typed fty (TRVar vb))) args
        | Just mi <- M.lookup (Res.symName vb) me ->
          let iT = resolveInstanceType mi fty
          in case iT of
            Ty.TyVar _ -> concatMap checkExpr args
            _ -> case lookupInstance ie (miClass mi) iT (Res.symName vb) of
              Just _  -> concatMap checkExpr args
              Nothing ->
                TypeError sp ("no instance of class " ++ T.unpack (miClass mi)
                             ++ " for type " ++ showType iT)
                : concatMap checkExpr args
      TRApp f as -> checkExpr f ++ concatMap checkExpr as
      TRLam _ _ b -> checkExpr b
      TRLet bs b -> concatMap (\(_, _, e) -> checkExpr e) bs ++ checkExpr b
      TRIf c t e -> checkExpr c ++ checkExpr t ++ checkExpr e
      TRCase s as -> checkExpr s ++ concatMap (\(_, e) -> checkExpr e) as
      _ -> []

    showType Ty.TyInt = "INT"
    showType Ty.TyFlt = "FLT"
    showType Ty.TyStr = "STR"
    showType Ty.TyBool = "BOOL"
    showType Ty.TyUnit = "UNIT"
    showType (Ty.TyCon n []) = T.unpack n
    showType (Ty.TyCon n ts) = T.unpack n ++ " " ++ unwords (map showType ts)
    showType (Ty.TyApp f a) = "(" ++ showType f ++ " " ++ showType a ++ ")"
    showType t = show t

-- | Haskell-style dictionary passing for typeclass methods.
-- Monomorphic calls: inline instance implementation (static dispatch).
-- Polymorphic calls: add dictionary parameters and extract methods from dicts.
dictPass :: ClassEnv -> MethodEnv -> InstanceEnv -> TResolvedCST -> TResolvedCST
dictPass classEnv methodEnv instanceEnv typed
  | M.null classEnv = typed
  | otherwise =
    let dictTypes = genDictTypes classEnv methodEnv
        dictBinds = genDictBinds classEnv methodEnv instanceEnv
        rewritten = map (dpRewrite classEnv methodEnv instanceEnv M.empty M.empty) typed
    in dictTypes ++ injectDictBinds dictBinds rewritten

typeToName :: Ty.Type -> T.Text
typeToName Ty.TyInt = "INT"
typeToName Ty.TyFlt = "FLT"
typeToName Ty.TyStr = "STR"
typeToName Ty.TyBool = "BOOL"
typeToName Ty.TyUnit = "UNIT"
typeToName Ty.TyRx = "RX"
typeToName (Ty.TyCon n []) = n
typeToName (Ty.TyCon n ts) = n <> "_" <> T.intercalate "_" (map typeToName ts)
typeToName (Ty.TyApp f a) = typeToName f <> "_" <> typeToName a
typeToName _ = "X"

classMethodOrder :: CST.Symbol -> MethodEnv -> [(CST.Symbol, MethodInfo)]
classMethodOrder className me =
  [(n, mi) | (n, mi) <- M.toList me, miClass mi == className]

-- Generate dictionary type declarations (one per class)
genDictTypes :: ClassEnv -> MethodEnv -> [TRExpr]
genDictTypes classEnv me =
  [mkDictType cn tvs me | (cn, tvs) <- M.toList classEnv]

mkDictType :: CST.Symbol -> [CST.Symbol] -> MethodEnv -> TRExpr
mkDictType className tvars me =
  let dn = "__DICT_" <> className
      cn = "__Dict" <> className
      pm = M.fromList (zip tvars [0..])
      methods = classMethodOrder className me
      fTys = [Ty.TyFun (map (rTV pm) (miArgTys mi)) (rTV pm (miRetTy mi))
             | (_, mi) <- methods]
      fNames = map fst methods
      ctor = CST.DataCon cn fTys (Just fNames)
  in Loc.Located dictSp (Ty.Typed (Ty.TyCon dn []) (TRType dn tvars [ctor]))
  where
    rTV pm (Ty.TyCon n []) | Just i <- M.lookup n pm = Ty.TyVar i
    rTV pm (Ty.TyCon n as) | Just i <- M.lookup n pm = foldl Ty.TyApp (Ty.TyVar i) (map (rTV pm) as)
    rTV pm (Ty.TyCon n as) = Ty.TyCon n (map (rTV pm) as)
    rTV pm (Ty.TyFun as r) = Ty.TyFun (map (rTV pm) as) (rTV pm r)
    rTV pm (Ty.TyApp f a) = Ty.TyApp (rTV pm f) (rTV pm a)
    rTV _ t = t

-- Generate dictionary instance let-bindings (one per instance)
genDictBinds :: ClassEnv -> MethodEnv -> InstanceEnv -> [(CST.Symbol, Ty.Type, TRExpr)]
genDictBinds ce me ie =
  concatMap (\(cn, insts) -> map (mkDictBind cn ce me ie) insts) (M.toList ie)

mkDictBind :: CST.Symbol -> ClassEnv -> MethodEnv -> InstanceEnv -> InstanceInfo -> (CST.Symbol, Ty.Type, TRExpr)
mkDictBind className ce me ie inst =
  let iTy = iiType inst
      dName = "__DICT_" <> className
      cName = "__Dict" <> className
      iName = "__inst_" <> className <> "_" <> typeToName iTy
      dTy = Ty.TyCon dName [iTy]
      tvars = M.findWithDefault [] className ce
      pm = M.fromList (zip tvars [0..])
      methods = classMethodOrder className me
      rawImpls = [case M.lookup mn (iiMethods inst) of
                     Just e -> e
                     Nothing -> error ("dictPass: missing " ++ T.unpack mn)
                 | (mn, _) <- methods]
      impls = map (dpRewrite ce me ie M.empty M.empty) rawImpls
      fTys = [Ty.TyFun (map (sTV pm iTy) (miArgTys mi)) (sTV pm iTy (miRetTy mi))
             | (_, mi) <- methods]
      cTy = if null fTys then dTy else Ty.TyFun fTys dTy
      cRef = Loc.Located dictSp (Ty.Typed cTy (TRVar (Res.VarBinding 0 cName)))
      app = if null impls then cRef
            else Loc.Located dictSp (Ty.Typed dTy (TRApp cRef impls))
  in (iName, dTy, app)
  where
    sTV pm ct (Ty.TyCon n []) | Just _ <- M.lookup n pm = ct
    sTV pm ct (Ty.TyCon n as) | Just _ <- M.lookup n pm = foldl Ty.TyApp ct (map (sTV pm ct) as)
    sTV pm ct (Ty.TyCon n as) = Ty.TyCon n (map (sTV pm ct) as)
    sTV pm ct (Ty.TyFun as r) = Ty.TyFun (map (sTV pm ct) as) (sTV pm ct r)
    sTV pm ct (Ty.TyApp f a) = Ty.TyApp (sTV pm ct f) (sTV pm ct a)
    sTV _ _ t = t

-- Inject dict bindings into the first TRLet
injectDictBinds :: [(CST.Symbol, Ty.Type, TRExpr)] -> TResolvedCST -> TResolvedCST
injectDictBinds [] es = es
injectDictBinds db es = go es
  where
    go [] = []
    go (Loc.Located sp (Ty.Typed ty (TRLet bs b)) : rest) =
      Loc.Located sp (Ty.Typed ty (TRLet (db ++ bs) b)) : rest
    go (e : rest) = e : go rest

-- Analyze a lambda body for class method calls on type variables
analyzeNeeds :: MethodEnv -> [(CST.Symbol, Ty.Type)] -> TRExpr -> [DictNeed]
analyzeNeeds me params body = dedup (go body)
  where
    dedup = foldr (\dn acc -> if any (sameDN dn) acc then acc else dn : acc) []
    sameDN a b = dnClass a == dnClass b && sameTyV (dnTyVar a) (dnTyVar b)
    sameTyV (Ty.TyVar a) (Ty.TyVar b) = a == b
    sameTyV _ _ = False

    go (Loc.Located _ (Ty.Typed _ node)) = case node of
      TRApp (Loc.Located _ (Ty.Typed fty (TRVar vb))) args ->
        (case M.lookup (Res.symName vb) me of
          Just mi ->
            let iT = resolveInstanceType mi fty
            in case iT of
              Ty.TyVar v -> case paramIdx v of
                Just idx -> [DictNeed (miClass mi) iT idx]
                Nothing -> []
              _ -> []
          Nothing -> []) ++ concatMap go args
      TRApp f as -> go f ++ concatMap go as
      TRLam _ _ b -> go b
      TRLet bs b -> concatMap (\(_, _, e) -> go e) bs ++ go b
      TRIf c t e -> go c ++ go t ++ go e
      TRCase s as -> go s ++ concatMap (\(_, e) -> go e) as
      _ -> []

    paramIdx v = lookup v [(n, i) | (i, (_, Ty.TyVar n)) <- zip [0..] params]

-- Main AST rewriter
dpRewrite :: ClassEnv -> MethodEnv -> InstanceEnv -> DictNeedsMap -> DictParamCtx -> TRExpr -> TRExpr
dpRewrite ce me ie nm dpc expr@(Loc.Located sp (Ty.Typed ty node)) = case node of
  TRApp fexpr args -> dpApp ce me ie nm dpc sp ty fexpr args
  TRLam params retTy body -> dpLam ce me ie nm dpc sp params retTy body
  TRLet binds body -> dpLetExpr ce me ie nm dpc sp ty binds body
  TRIf c t e -> Loc.Located sp (Ty.Typed ty (TRIf (rw c) (rw t) (rw e)))
  TRCase scr arms -> Loc.Located sp (Ty.Typed ty (TRCase (rw scr) [(p, rw e) | (p, e) <- arms]))
  _ -> expr
  where rw = dpRewrite ce me ie nm dpc

-- Rewrite function application: method calls and dict-parameterized function calls
dpApp :: ClassEnv -> MethodEnv -> InstanceEnv -> DictNeedsMap -> DictParamCtx
      -> Loc.Span -> Ty.Type -> TRExpr -> [TRExpr] -> TRExpr
dpApp ce me ie nm dpc sp ty fexpr args =
  let args' = map (dpRewrite ce me ie nm dpc) args
  in case fexpr of
    Loc.Located fsp (Ty.Typed fty (TRVar vb))
      -- Class method call
      | Just mi <- M.lookup (Res.symName vb) me ->
        let iT = resolveInstanceType mi fty
        in case iT of
          Ty.TyVar v ->
            -- Polymorphic: extract from dict param
            case M.lookup (miClass mi, v) dpc of
              Just dpn ->
                let ext = mkMethodExtract sp dpn (miClass mi) (Res.symName vb) iT ce me
                in Loc.Located sp (Ty.Typed ty (TRApp ext args'))
              Nothing -> Loc.Located sp (Ty.Typed ty (TRApp fexpr args'))
          _ ->
            -- Monomorphic: inline instance impl (static dispatch)
            case lookupInstance ie (miClass mi) iT (Res.symName vb) of
              Just impl ->
                let impl' = dpRewrite ce me ie nm dpc impl
                    Loc.Located _ (Ty.Typed _ implBody) = impl'
                in Loc.Located sp (Ty.Typed ty (TRApp (Loc.Located fsp (Ty.Typed fty implBody)) args'))
              Nothing -> Loc.Located sp (Ty.Typed ty (TRApp fexpr args'))

      -- Call to a dict-parameterized function
      | Just needs <- M.lookup (Res.symName vb) nm ->
        let dArgs = map (mkDictArgForCall ie dpc fty) needs
            allArgs = dArgs ++ args'
            newFty = Ty.TyFun (map typeOf allArgs) ty
            newF = Loc.Located fsp (Ty.Typed newFty (TRVar vb))
        in Loc.Located sp (Ty.Typed ty (TRApp newF allArgs))

    -- Regular call
    _ -> Loc.Located sp (Ty.Typed ty (TRApp (dpRewrite ce me ie nm dpc fexpr) args'))

-- Rewrite lambda: add dict params if body uses class methods on type variables
dpLam :: ClassEnv -> MethodEnv -> InstanceEnv -> DictNeedsMap -> DictParamCtx
      -> Loc.Span -> [(CST.Symbol, Ty.Type)] -> Ty.Type -> TRExpr -> TRExpr
dpLam ce me ie nm dpc sp params retTy body =
  let needs = analyzeNeeds me params body
  in if null needs
     then
       let body' = dpRewrite ce me ie nm dpc body
       in Loc.Located sp (Ty.Typed (Ty.TyFun (map snd params) retTy) (TRLam params retTy body'))
     else
       let dParams = [(dpNameFor dn, dpTypeFor dn) | dn <- needs]
           newDpc = foldl (\ctx dn -> case dnTyVar dn of
                             Ty.TyVar v -> M.insert (dnClass dn, v) (dpNameFor dn) ctx
                             _ -> ctx) dpc needs
           body' = dpRewrite ce me ie nm newDpc body
           allParams = dParams ++ params
           newTy = Ty.TyFun (map snd allParams) retTy
       in Loc.Located sp (Ty.Typed newTy (TRLam allParams retTy body'))

-- Rewrite let: build needs map, rewrite bindings and body
dpLetExpr :: ClassEnv -> MethodEnv -> InstanceEnv -> DictNeedsMap -> DictParamCtx
          -> Loc.Span -> Ty.Type -> [(CST.Symbol, Ty.Type, TRExpr)] -> TRExpr -> TRExpr
dpLetExpr ce me ie parentNm dpc sp ty binds body =
  let localNeeds = M.fromList
        [(name, needs)
        | (name, _, Loc.Located _ (Ty.Typed _ (TRLam params _ lb))) <- binds
        , let needs = analyzeNeeds me params lb
        , not (null needs)]
      nm = M.union localNeeds parentNm
      binds' = map (dpBind ce me ie nm dpc) binds
      body' = dpRewrite ce me ie nm dpc body
  in Loc.Located sp (Ty.Typed ty (TRLet binds' body'))

dpBind :: ClassEnv -> MethodEnv -> InstanceEnv -> DictNeedsMap -> DictParamCtx
       -> (CST.Symbol, Ty.Type, TRExpr) -> (CST.Symbol, Ty.Type, TRExpr)
dpBind ce me ie nm dpc (name, ty, Loc.Located sp (Ty.Typed lamTy (TRLam params retTy body))) =
  let needs = analyzeNeeds me params body
  in if null needs
     then (name, ty, dpRewrite ce me ie nm dpc (Loc.Located sp (Ty.Typed lamTy (TRLam params retTy body))))
     else
       let dParams = [(dpNameFor dn, dpTypeFor dn) | dn <- needs]
           newDpc = foldl (\ctx dn -> case dnTyVar dn of
                             Ty.TyVar v -> M.insert (dnClass dn, v) (dpNameFor dn) ctx
                             _ -> ctx) dpc needs
           body' = dpRewrite ce me ie nm newDpc body
           allParams = dParams ++ params
           newTy = Ty.TyFun (map snd allParams) retTy
       in (name, newTy, Loc.Located sp (Ty.Typed newTy (TRLam allParams retTy body')))
dpBind ce me ie nm dpc (name, ty, expr) =
  (name, ty, dpRewrite ce me ie nm dpc expr)

dpNameFor :: DictNeed -> CST.Symbol
dpNameFor dn = "__dict_" <> dnClass dn

dpTypeFor :: DictNeed -> Ty.Type
dpTypeFor dn = Ty.TyCon ("__DICT_" <> dnClass dn) [dnTyVar dn]

-- Extract a method from a dictionary via case pattern match
mkMethodExtract :: Loc.Span -> CST.Symbol -> CST.Symbol -> CST.Symbol -> Ty.Type
               -> ClassEnv -> MethodEnv -> TRExpr
mkMethodExtract sp dictName className methodName tyVar ce me =
  let dtn = "__DICT_" <> className
      ctn = "__Dict" <> className
      dTy = Ty.TyCon dtn [tyVar]
      tvars = M.findWithDefault [] className ce
      pm = M.fromList (zip tvars [0..])
      methods = classMethodOrder className me
      mIdx = findIdx methodName (map fst methods)
      fTys = [Ty.TyFun (map (rTV pm tyVar) (miArgTys mi)) (rTV pm tyVar (miRetTy mi))
             | (_, mi) <- methods]
      mTy = fTys !! mIdx
      pats = [if i == mIdx then TRPatVar "__m" ft else TRPatWild ft
             | (i, ft) <- zip [0..] fTys]
      pat = TRPatCon ctn dTy pats
      scr = Loc.Located sp (Ty.Typed dTy (TRVar (Res.VarBinding 0 dictName)))
      bdy = Loc.Located sp (Ty.Typed mTy (TRVar (Res.VarBinding 0 "__m")))
  in Loc.Located sp (Ty.Typed mTy (TRCase scr [(pat, bdy)]))
  where
    rTV pm tv (Ty.TyCon n []) | Just _ <- M.lookup n pm = tv
    rTV pm tv (Ty.TyCon n as) = Ty.TyCon n (map (rTV pm tv) as)
    rTV pm tv (Ty.TyFun as r) = Ty.TyFun (map (rTV pm tv) as) (rTV pm tv r)
    rTV _ _ t = t
    findIdx x xs = go' 0 xs where
      go' _ [] = 0
      go' i (y:ys) | x == y = i | otherwise = go' (i+1) ys

-- Create a dict argument reference for a call site
mkDictArgForCall :: InstanceEnv -> DictParamCtx -> Ty.Type -> DictNeed -> TRExpr
mkDictArgForCall _ie dpc callFty dn =
  case callFty of
    Ty.TyFun argTys _ | dnParamIdx dn < length argTys ->
      let cTy = argTys !! dnParamIdx dn
      in case cTy of
        Ty.TyVar v ->
          -- Still polymorphic at call site: pass through our dict param
          case M.lookup (dnClass dn, v) dpc of
            Just dpn ->
              let dictTy = Ty.TyCon ("__DICT_" <> dnClass dn) [cTy]
              in Loc.Located dictSp (Ty.Typed dictTy (TRVar (Res.VarBinding 0 dpn)))
            Nothing -> error ("dictPass: no dict for " ++ T.unpack (dnClass dn))
        _ ->
          -- Concrete type: reference the instance dict
          let iName = "__inst_" <> dnClass dn <> "_" <> typeToName cTy
              dictTy = Ty.TyCon ("__DICT_" <> dnClass dn) [cTy]
          in Loc.Located dictSp (Ty.Typed dictTy (TRVar (Res.VarBinding 0 iName)))
    _ -> error "dictPass: unexpected function type at call site"

-- | Determine the concrete type for the class type variable from the resolved function type.
-- Scans both args and return type for the class variable position.
resolveInstanceType :: MethodInfo -> Ty.Type -> Ty.Type
resolveInstanceType minfo fty = case fty of
  Ty.TyFun instArgs instRet ->
    let cvs = S.fromList (miClassVars minfo)
        origParts = miArgTys minfo ++ [miRetTy minfo]
        instParts = instArgs ++ [instRet]
    in findClassVar cvs origParts instParts
  _ -> fty
  where
    findClassVar _ [] _ = fty
    findClassVar _ _ [] = fty
    findClassVar cvs (orig:origs) (inst:insts) =
      case containsClassVar cvs orig of
        Just False -> findClassVar cvs origs insts
        Just True  -> inst
        Nothing    -> extractHead inst
    containsClassVar cvs (Ty.TyCon name []) | S.member name cvs = Just True
    containsClassVar cvs (Ty.TyCon name _) | S.member name cvs = Nothing
    containsClassVar cvs (Ty.TyCon _ args) | any (\a -> containsClassVar cvs a /= Just False) args = Nothing
    containsClassVar _ _ = Just False
    extractHead (Ty.TyCon name (_:_)) = Ty.TyCon name []
    extractHead (Ty.TyApp f _) = extractHead f
    extractHead t = t

-- | Look up an instance for a class and type, returning a specific method.
-- Supports parametric instances: type variables in the instance pattern match any type.
lookupInstance :: InstanceEnv -> CST.Symbol -> Ty.Type -> CST.Symbol -> Maybe TRExpr
lookupInstance ienv className instTy methodName =
  case M.lookup className ienv of
    Nothing -> Nothing
    Just insts ->
      case filter (\ii -> matchInstanceType (iiTyVars ii) (iiType ii) instTy) insts of
        (ii : _) -> M.lookup methodName (iiMethods ii)
        [] -> Nothing

-- | Structural matching of an instance type pattern against a target type.
-- TyCon names in the tyVars set act as wildcards (match any type).
matchInstanceType :: S.Set CST.Symbol -> Ty.Type -> Ty.Type -> Bool
matchInstanceType tyVars pat target = case (pat, target) of
  (Ty.TyCon n [], _) | S.member n tyVars -> True
  (Ty.TyCon n1 as1, Ty.TyCon n2 as2) ->
    n1 == n2 && length as1 == length as2
    && all (uncurry (matchInstanceType tyVars)) (zip as1 as2)
  (Ty.TyFun as1 r1, Ty.TyFun as2 r2) ->
    length as1 == length as2
    && all (uncurry (matchInstanceType tyVars)) (zip as1 as2)
    && matchInstanceType tyVars r1 r2
  (Ty.TyApp f1 a1, Ty.TyApp f2 a2) ->
    matchInstanceType tyVars f1 f2 && matchInstanceType tyVars a1 a2
  _ -> pat == target

-- INFERENCE

infer :: Res.RExpr -> Infer TRExpr
infer (Loc.Located sp expr) = Loc.Located sp <$> case expr of
  Res.RLit l@(CST.LitInt _) ->
    pure $ Ty.Typed Ty.TyInt (TRLit l)
  Res.RLit l@(CST.LitFlt _) ->
    pure $ Ty.Typed Ty.TyFlt (TRLit l)
  Res.RLit l@(CST.LitStr _) ->
    pure $ Ty.Typed Ty.TyStr (TRLit l)
  Res.RLit l@(CST.LitRx _ _) ->
    pure $ Ty.Typed Ty.TyRx (TRLit l)
  Res.RBool b ->
    pure $ Ty.Typed Ty.TyBool (TRBool b)
  Res.RUnit ->
    pure $ Ty.Typed Ty.TyUnit TRUnit
  Res.RVar vb -> do
    ctx <- askCtx
    case M.lookup (Res.symName vb) ctx of
      Just scheme -> do
        t <- instantiate scheme
        pure $ Ty.Typed t (TRVar vb)
      Nothing -> do
        t <- recordError sp ("Undefined variable " ++ show (Res.symName vb))
        pure $ Ty.Typed t (TRVar vb)
  Res.RIf c t e -> do
    ct <- infer c
    tt <- infer t
    et <- infer e
    constrain sp (typeOf ct) Ty.TyBool
    constrain sp (typeOf tt) (typeOf et)
    pure $ Ty.Typed (typeOf tt) (TRIf ct tt et)
  Res.RApp fexpr aexprs -> do
    ft <- infer fexpr
    -- Check for keyword args: separate them from positional
    let (posExprs, keyExprs) = partitionArgs aexprs
    posAts <- traverse infer posExprs
    -- Look up function metadata for call-site rewriting
    funcInfo <- lookupFuncInfo fexpr
    variadics <- ieVariadics <$> RWS.ask
    let isVariadic = case fexpr of
          Loc.Located _ (Res.RVar vb) -> S.member (Res.symName vb) variadics
          _ -> False
        reqArity = case funcInfo of
          Just (FuncInfo ra _) -> ra
          Nothing -> case typeOf ft of
            Ty.TyFun ps _ -> length ps
            _             -> 0

    -- Partial application: fewer positional args than required arity
    case typeOf ft of
      Ty.TyFun paramTys retTy
        | not isVariadic, not (null posAts), null keyExprs
        , length posAts < reqArity -> do
          let nSupplied      = length posAts
              suppliedTys    = take nSupplied paramTys
              remainingReqTys = take (reqArity - nSupplied) (drop nSupplied paramTys)
          sequence_ [constrain sp (typeOf a) pt | (a, pt) <- zip posAts suppliedTys]
          -- Fresh params for remaining required args
          n <- RWS.get
          let nRemaining = length remainingReqTys
              freshNames = [T.pack ("$$pa" ++ show (n + fromIntegral i)) | i <- [0..nRemaining-1]]
          RWS.put (n + fromIntegral nRemaining)
          let freshParams = zip freshNames remainingReqTys
              freshVars   = [Loc.Located sp (Ty.Typed rty (TRVar (Res.VarBinding 0 name)))
                            | (name, rty) <- freshParams]
          -- Bake in defaults/rest for non-required params
          extraArgs <- case funcInfo of
            Just (FuncInfo _ (FIOpt defaults)) -> traverse infer defaults
            Just (FuncInfo _ FIRest) -> do
              elemTy <- fresh
              restExpr <- buildListExpr sp elemTy []
              pure [restExpr]
            Just (FuncInfo _ (FIKey _ defaults)) -> traverse infer defaults
            _ -> pure []
          let innerArgs  = posAts ++ freshVars ++ extraArgs
              innerApp   = Loc.Located sp (Ty.Typed retTy (TRApp ft innerArgs))
              curryRetTy = Ty.TyFun remainingReqTys retTy
          pure $ Ty.Typed curryRetTy (TRLam freshParams retTy innerApp)

      _ -> do
        -- Normal call: funcInfo dispatch for defaults/rest/keys
        ats <- case funcInfo of
          Just (FuncInfo ra (FIOpt defaults)) -> do
            let nPos = length posAts
                totalArity = ra + length defaults
            if nPos >= totalArity then pure posAts
            else if nPos < ra then pure posAts
            else do
              let nMissing = totalArity - nPos
                  missingDefaults = drop (nPos - ra) defaults
              defaultAts <- traverse infer (take nMissing missingDefaults)
              pure (posAts ++ defaultAts)
          Just (FuncInfo ra FIRest) -> do
            let nPos = length posAts
            if nPos < ra then pure posAts
            else do
              let reqArgs  = take ra posAts
                  restArgs = drop ra posAts
              elemTy <- if null restArgs then fresh
                        else pure (typeOf (head restArgs))
              listExpr <- buildListExpr sp elemTy restArgs
              pure (reqArgs ++ [listExpr])
          Just (FuncInfo _ (FIKey keyNames defaults)) -> do
            keyAts <- traverse (\(_, e) -> infer e) keyExprs
            let keyMap = M.fromList (zip (map fst keyExprs) keyAts)
            reorderedKeys <- sequence
              [ case M.lookup name keyMap of
                  Just expr -> pure expr
                  Nothing   -> infer (defaults !! idx)
              | (idx, name) <- zip [0..] keyNames
              ]
            pure (posAts ++ reorderedKeys)
          _ -> do
            if null keyExprs then pure posAts
            else do
              keyAts <- traverse (\(_, e) -> infer e) keyExprs
              pure (posAts ++ keyAts)
        rt <- fresh
        if isVariadic
          then case typeOf ft of
            Ty.TyFun fixedTys fRet -> do
              sequence_ [constrain sp (typeOf a) ft' | (a, ft') <- zip ats fixedTys]
              constrain sp fRet rt
            _ -> constrain sp (typeOf ft) (Ty.TyFun (map typeOf ats) rt)
          else constrain sp (typeOf ft) (Ty.TyFun (map typeOf ats) rt)
        pure $ Ty.Typed rt (TRApp ft ats)

  Res.RKeyArg _ _ -> do
    t <- recordError sp "&key argument outside function application"
    pure $ Ty.Typed t TRUnit
  Res.RLam (Res.RLamList required rExtra) _mRet body -> do
    reqTys <- traverse paramType required
    let reqNames = map CST.symName required
    -- Build the full param list including extra params
    (extraNames, extraTys, extraDefaults) <- inferExtra sp rExtra
    let allNames = reqNames ++ extraNames
        allTys   = reqTys ++ extraTys
        paramSchemes = map (Forall S.empty) allTys
        newBindings = M.fromList (zip allNames paramSchemes)
    -- Type-check default expressions to ensure consistency
    mapM_ (\(defExpr, expectedTy) -> do
      tdef <- localCtx (M.union newBindings) (infer defExpr)
      constrain sp (typeOf tdef) expectedTy
      ) extraDefaults
    bodyExpr <- localCtx (M.union newBindings) (infer body)
    let funTy = Ty.TyFun allTys (typeOf bodyExpr)
    pure $ Ty.Typed funTy (TRLam (zip allNames allTys) (typeOf bodyExpr) bodyExpr)
  Res.RLet binds body -> do
    ctx <- askCtx
    let names = map (CST.symName . fst) binds
    freshTys <- traverse (const fresh) names
    let monoSchemes = map (Forall S.empty) freshTys
        recCtx = M.union (M.fromList (zip names monoSchemes)) ctx
    -- Collect func metadata from lambda bindings
    let funcMetas = collectFuncMetas (zip names (map snd binds))
    (rhsExprs, (rhsConstraints, _)) <- RWS.listen $
      localFuncs (M.union funcMetas) $
      traverse (\(_, rhs) -> localCtx (const recCtx) (infer rhs)) binds
    sequence_ $ zipWith (constrain sp) freshTys (map typeOf rhsExprs)
    -- Solve RHS constraints eagerly so generalization uses concrete types,
    -- not raw inference variables (fixes over-generalization of applied fns).
    let eagerConstraints = rhsConstraints ++ zipWith (Constraint sp) freshTys (map typeOf rhsExprs)
        concreteFreshTys  = case solveAll eagerConstraints of
                              Right subst -> map (apply subst) freshTys
                              Left _      -> map typeOf rhsExprs
        generalizedSchemes = map (generalize ctx) concreteFreshTys
        bodyCtx = M.union (M.fromList (zip names generalizedSchemes)) ctx
    bodyExpr <- localFuncs (M.union funcMetas) $
      localCtx (const bodyCtx) (infer body)
    let typedBinds = zip3 names (map typeOf rhsExprs) rhsExprs
    pure $ Ty.Typed (typeOf bodyExpr) (TRLet typedBinds bodyExpr)
  Res.RCls name _tvars _methods ->
    -- Class info is extracted in pre-pass, just pass through
    pure $ Ty.Typed Ty.TyUnit TRUnit
  Res.RInst _className _ty _methods ->
    -- Instance info is extracted in pre-pass, just pass through
    pure $ Ty.Typed Ty.TyUnit TRUnit
  Res.RType name params ctors ->
    -- Constructors are registered in context by typecheck, just pass through here
    pure $ Ty.Typed (Ty.TyCon name []) (TRType name params ctors)
  Res.RFFI name paramTys retTy ->
    pure $ Ty.Typed Ty.TyUnit (TRFFI name paramTys retTy)
  Res.RFFIStruct name fields ->
    pure $ Ty.Typed Ty.TyUnit (TRFFIStruct name fields)
  Res.RFFIVar name paramTys retTy ->
    pure $ Ty.Typed Ty.TyUnit (TRFFIVar name paramTys retTy)
  Res.RFFIEnum name variants ->
    pure $ Ty.Typed Ty.TyUnit (TRFFIEnum name variants)
  Res.RFFICallback name paramTys retTy ->
    pure $ Ty.Typed Ty.TyUnit (TRFFICallback name paramTys retTy)
  Res.RFieldAccess fieldName subExpr -> do
    scrutExpr <- infer subExpr
    let scrutTy = typeOf scrutExpr
    fmap <- ieFields <$> RWS.ask
    structFields <- ieStructFields <$> RWS.ask
    case M.lookup fieldName fmap of
      Nothing -> do
        t <- recordError sp ("no field '" ++ T.unpack fieldName ++ "'")
        pure $ Ty.Typed t TRUnit
      Just (ctorName, fieldIdx, numFields) ->
        -- For FFI structs: use full field list (includes arrays)
        case M.lookup ctorName structFields of
          Just allFields -> do
            let resultTy = Ty.TyCon ctorName []
            constrain sp scrutTy resultTy
            let allFieldTys = map (Ty.cTypeToPllisp . snd) allFields
                fieldTy = allFieldTys !! fieldIdx
                pats = [if i == fieldIdx
                        then TRPatVar fieldName ft
                        else TRPatWild ft
                       | (i, ft) <- zip [0..] allFieldTys]
                pat = TRPatCon ctorName scrutTy pats
                binding = Res.VarBinding 0 fieldName
                body = Loc.Located sp (Ty.Typed fieldTy (TRVar binding))
            pure $ Ty.Typed fieldTy (TRCase scrutExpr [(pat, body)])
          Nothing -> do
            -- Regular ADT field access
            ctx <- askCtx
            ctorTy <- case M.lookup ctorName ctx of
              Just scheme -> instantiate scheme
              Nothing     -> recordError sp ("unknown constructor " ++ T.unpack ctorName)
            let (argTys, resultTy) = case ctorTy of
                  Ty.TyFun as r -> (as, r)
                  t             -> ([], t)
            constrain sp scrutTy resultTy
            if fieldIdx >= length argTys || numFields /= length argTys
              then do
                t <- recordError sp ("field index out of bounds for " ++ T.unpack fieldName)
                pure $ Ty.Typed t TRUnit
              else do
                let fieldTy = argTys !! fieldIdx
                    pats = [if i == fieldIdx
                            then TRPatVar fieldName ft
                            else TRPatWild ft
                           | (i, ft) <- zip [0..] argTys]
                    pat = TRPatCon ctorName scrutTy pats
                    binding = Res.VarBinding 0 fieldName
                    body = Loc.Located sp (Ty.Typed fieldTy (TRVar binding))
                pure $ Ty.Typed fieldTy (TRCase scrutExpr [(pat, body)])
  Res.RCase scrutinee arms -> do
    scrutExpr <- infer scrutinee
    let scrutTy = typeOf scrutExpr
    resultTy <- fresh
    rarms <- traverse (inferArm scrutTy resultTy) arms
    pure $ Ty.Typed resultTy (TRCase scrutExpr rarms)
    where
      inferArm scrutTy resultTy (pat, body) = do
        (rpat, bindings) <- inferPattern scrutTy pat sp
        bodyExpr <- localCtx (M.union (M.fromList bindings)) (infer body)
        constrain sp (typeOf bodyExpr) resultTy
        pure (rpat, bodyExpr)

-- Inference helpers

fresh :: Infer Ty.Type
fresh = RWS.state (\n -> (Ty.TyVar n, n+1))

constrain :: Loc.Span -> Ty.Type -> Ty.Type -> Infer ()
constrain sp t1 t2 = RWS.tell ([Constraint sp t1 t2], [])

-- | Record a type error and return a fresh type variable as placeholder
recordError :: Loc.Span -> String -> Infer Ty.Type
recordError sp msg = do
  RWS.tell ([], [TypeError sp msg])
  fresh

paramType :: CST.TSymbol -> Infer Ty.Type
paramType tsym = case CST.symType tsym of
  Just t  -> pure t
  Nothing -> fresh

typeOf :: TRExpr -> Ty.Type
typeOf (Loc.Located _ (Ty.Typed t _)) = t

generalize :: Context -> Ty.Type -> Scheme
generalize ctx t = Forall (tvs t `S.difference` tvs (M.elems ctx)) t

instantiate :: Scheme -> Infer Ty.Type
instantiate (Forall vs t) = do
  let vars = S.toList vs
  ftvs <- traverse (const fresh) vars
  let subst = M.fromList (zip vars ftvs)
  return $ apply subst t

-- Extended lambda list helpers

-- | Infer types and collect defaults for the extra part of a lambda list.
-- Returns: (param names, param types, [(default RExpr, expected type)])
inferExtra :: Loc.Span -> Res.RLamExtra -> Infer ([CST.Symbol], [Ty.Type], [(Res.RExpr, Ty.Type)])
inferExtra _ Res.RNoExtra = pure ([], [], [])
inferExtra _ (Res.RRestParam ts) = do
  elemTy <- case CST.symType ts of
    Just (Ty.TyCon "LIST" [t]) -> pure t
    Just _ -> fresh  -- let unification catch mismatches
    Nothing -> fresh
  let listTy = Ty.TyCon "LIST" [elemTy]
  pure ([CST.symName ts], [listTy], [])
inferExtra _ (Res.ROptParams opts) = do
  tys <- traverse (\(ts, _) -> paramType ts) opts
  let names = map (CST.symName . fst) opts
      defaults = [(defExpr, ty) | ((_, defExpr), ty) <- zip opts tys]
  pure (names, tys, defaults)
inferExtra _ (Res.RKeyParams keys) = do
  tys <- traverse (\(ts, _) -> paramType ts) keys
  let names = map (CST.symName . fst) keys
      defaults = [(defExpr, ty) | ((_, defExpr), ty) <- zip keys tys]
  pure (names, tys, defaults)

-- | Collect FuncInfo metadata from let-bindings where the RHS is a lambda.
collectFuncMetas :: [(CST.Symbol, Res.RExpr)] -> FuncInfoMap
collectFuncMetas = M.fromList . concatMap go
  where
    go (name, Loc.Located _ (Res.RLam (Res.RLamList required extra) _ _)) =
      let reqArity = length required
          extraInfo = case extra of
            Res.RNoExtra -> FIPlain
            Res.ROptParams opts -> FIOpt (map snd opts)
            Res.RRestParam _   -> FIRest
            Res.RKeyParams keys -> FIKey (map (CST.symName . fst) keys) (map snd keys)
      in [(name, FuncInfo reqArity extraInfo)]
    go _ = []

-- | Look up FuncInfo for a function expression (only works for named variables).
lookupFuncInfo :: Res.RExpr -> Infer (Maybe FuncInfo)
lookupFuncInfo (Loc.Located _ (Res.RVar vb)) = do
  funcs <- ieFuncs <$> RWS.ask
  pure $ M.lookup (Res.symName vb) funcs
lookupFuncInfo _ = pure Nothing

-- | Partition application args into positional and keyword args.
partitionArgs :: [Res.RExpr] -> ([Res.RExpr], [(CST.Symbol, Res.RExpr)])
partitionArgs = go [] []
  where
    go posAcc keyAcc [] = (reverse posAcc, reverse keyAcc)
    go posAcc keyAcc (Loc.Located _ (Res.RKeyArg name val) : rest) =
      go posAcc ((name, val) : keyAcc) rest
    go posAcc keyAcc (arg : rest) =
      go (arg : posAcc) keyAcc rest

-- | Build a Cons/Nil list expression from a list of typed expressions.
buildListExpr :: Loc.Span -> Ty.Type -> [TRExpr] -> Infer TRExpr
buildListExpr sp elemTy [] = do
  let listTy = Ty.TyCon "LIST" [elemTy]
  pure $ Loc.Located sp (Ty.Typed listTy (TRVar (Res.VarBinding 0 "NIL")))
buildListExpr sp elemTy (x:xs) = do
  rest <- buildListExpr sp elemTy xs
  let listTy = Ty.TyCon "LIST" [elemTy]
      consTy = Ty.TyFun [elemTy, listTy] listTy
      consFn = Loc.Located sp (Ty.Typed consTy (TRVar (Res.VarBinding 0 "CONS")))
  constrain sp (typeOf x) elemTy
  pure $ Loc.Located sp (Ty.Typed listTy (TRApp consFn [x, rest]))

inferPattern :: Ty.Type -> Res.RPattern -> Loc.Span -> Infer (TRPattern, [(CST.Symbol, Scheme)])
inferPattern ty pat sp = case pat of
  Res.RPatLit l -> do
    let litTy = case l of
          CST.LitInt _     -> Ty.TyInt
          CST.LitFlt _     -> Ty.TyFlt
          CST.LitStr _     -> Ty.TyStr
          CST.LitRx _ _ -> Ty.TyRx
    constrain sp ty litTy
    pure (TRPatLit l, [])
  Res.RPatBool b -> do
    constrain sp ty Ty.TyBool
    pure (TRPatBool b, [])
  Res.RPatWild ->
    pure (TRPatWild ty, [])
  Res.RPatVar s ->
    pure (TRPatVar s ty, [(s, Forall S.empty ty)])
  Res.RPatCon ctor subpats -> do
    ctx <- askCtx
    ctorTy <- case M.lookup ctor ctx of
      Just scheme -> instantiate scheme
      Nothing     -> recordError sp ("Unknown constructor in pattern: " ++ show ctor)
    let (argTys, resultTy) = case ctorTy of
          Ty.TyFun as r -> (as, r)
          t             -> ([], t)
    constrain sp ty resultTy
    if length argTys /= length subpats
      then do
        _ <- recordError sp ("Constructor arity mismatch: " ++ show ctor)
        pure (TRPatCon ctor ty [], [])
      else do
        (rpats, bindingss) <- unzip <$> zipWithM (\argTy p -> inferPattern argTy p sp) argTys subpats
        pure (TRPatCon ctor ty rpats, concat bindingss)

zipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM f xs ys = sequence (zipWith f xs ys)

-- UNIFICATION / SOLVING

unify :: Loc.Span -> Ty.Type -> Ty.Type -> Either [TypeError] Subst
unify sp (Ty.TyVar v) t = bind sp v t
unify sp t (Ty.TyVar v) = bind sp v t
unify sp (Ty.TyFun as1 r1) (Ty.TyFun as2 r2) = unifyMany sp (r1:as1) (r2:as2)
unify sp (Ty.TyCon n1 ts1) (Ty.TyCon n2 ts2)
  | n1 == n2  = unifyMany sp ts1 ts2
unify sp (Ty.TyApp f1 a1) (Ty.TyApp f2 a2) = do
  s1 <- unify sp f1 f2
  s2 <- unify sp (apply s1 a1) (apply s1 a2)
  Right (compose s2 s1)
-- TyApp vs TyCon: decompose TyCon "F" [a,b] into TyApp (TyCon "F" [a]) b
unify sp ta@(Ty.TyApp _ _) (Ty.TyCon name args@(_:_)) =
  unify sp ta (Ty.TyApp (Ty.TyCon name (init args)) (last args))
unify sp (Ty.TyCon name args@(_:_)) ta@(Ty.TyApp _ _) =
  unify sp (Ty.TyApp (Ty.TyCon name (init args)) (last args)) ta
unify _ Ty.TyInt Ty.TyInt = Right M.empty
unify _ Ty.TyFlt Ty.TyFlt = Right M.empty
unify _ Ty.TyStr Ty.TyStr = Right M.empty
unify _ Ty.TyBool Ty.TyBool = Right M.empty
unify _ Ty.TyUnit Ty.TyUnit = Right M.empty
unify _ Ty.TyRx Ty.TyRx = Right M.empty
unify sp t1 t2 = Left [TypeError sp ("cannot unify " ++ show t1 ++ " with " ++ show t2)]

-- | Unify multiple type pairs, collecting all errors
unifyMany :: Loc.Span -> [Ty.Type] -> [Ty.Type] -> Either [TypeError] Subst
unifyMany _ [] [] = Right M.empty
unifyMany sp (t1:ts1) (t2:ts2) =
  case unify sp t1 t2 of
    Left errs1 ->
      -- Continue unifying rest to collect more errors
      case unifyMany sp ts1 ts2 of
        Left errs2 -> Left (errs1 ++ errs2)
        Right _    -> Left errs1
    Right s1 ->
      case unifyMany sp (apply s1 ts1) (apply s1 ts2) of
        Left errs -> Left errs
        Right s2  -> Right (compose s2 s1)
unifyMany sp _ _ = Left [TypeError sp "type mismatch: different arities"]

bind :: Loc.Span -> TyVar -> Ty.Type -> Either [TypeError] Subst
bind sp tv t
  | tv `S.member` tvs t = Left [TypeError sp ("infinite type " ++ show tv ++ " ~ " ++ show t)]
  | otherwise = Right $ M.singleton tv t

solve :: Constraints -> Either [TypeError] Subst
solve [] = Right M.empty
solve (Constraint sp t1 t2 : cs) = do
  s1 <- unify sp t1 t2
  s2 <- solve (apply s1 cs)
  Right (compose s2 s1)

-- | Solve all constraints, collecting all type errors instead of failing on the first
solveAll :: Constraints -> Either [TypeError] Subst
solveAll [] = Right M.empty
solveAll constraints = go M.empty constraints []
  where
    go subst [] errs
      | null errs = Right subst
      | otherwise = Left (reverse errs)
    go subst (Constraint sp t1 t2 : cs) errs =
      case unify sp (apply subst t1) (apply subst t2) of
        Left es -> go subst cs (errs ++ es)
        Right s -> go (compose s subst) cs errs

-- TAIL CALL OPTIMIZATION

tcoPass :: TResolvedCST -> TResolvedCST
tcoPass = map tcoExpr

tcoExpr :: TRExpr -> TRExpr
tcoExpr (Loc.Located sp (Ty.Typed t expr)) =
  Loc.Located sp (Ty.Typed t (tcoExprF expr))

tcoExprF :: TRExprF -> TRExprF
tcoExprF (TRLet binds body) =
  TRLet (map tcoBind binds) (tcoExpr body)
tcoExprF (TRLam ps rt body) = TRLam ps rt (tcoExpr body)
tcoExprF (TRIf c t e) = TRIf (tcoExpr c) (tcoExpr t) (tcoExpr e)
tcoExprF (TRApp f as) = TRApp (tcoExpr f) (map tcoExpr as)
tcoExprF (TRCase s arms) = TRCase (tcoExpr s) [(p, tcoExpr b) | (p, b) <- arms]
tcoExprF e = e

tcoBind :: (CST.Symbol, Ty.Type, TRExpr) -> (CST.Symbol, Ty.Type, TRExpr)
tcoBind (name, ty, rhs) = case rhs of
  Loc.Located sp (Ty.Typed lamTy (TRLam params retTy body))
    | hasSelfTailCall name body ->
      let body' = rewriteTailCalls name body
          bsp = case body of Loc.Located s _ -> s
          bty = case body of Loc.Located _ (Ty.Typed t _) -> t
      in (name, ty, Loc.Located sp (Ty.Typed lamTy
           (TRLam params retTy
             (Loc.Located bsp (Ty.Typed bty (TRLoop params body'))))))
  _ -> (name, ty, tcoExpr rhs)

-- | Check if an expression has a self-recursive tail call to the given name.
hasSelfTailCall :: CST.Symbol -> TRExpr -> Bool
hasSelfTailCall name (Loc.Located _ (Ty.Typed _ expr)) = case expr of
  TRApp (Loc.Located _ (Ty.Typed _ (TRVar vb))) _ -> Res.symName vb == name
  TRIf _ t e -> hasSelfTailCall name t || hasSelfTailCall name e
  TRLet binds body ->
    not (any (\(n, _, _) -> n == name) binds) && hasSelfTailCall name body
  TRCase _ arms -> any (\(_, b) -> hasSelfTailCall name b) arms
  _ -> False

-- | Rewrite self-recursive tail calls to TRRecur, non-tail subexpressions
-- are recursed into with tcoExpr for nested TCO opportunities.
rewriteTailCalls :: CST.Symbol -> TRExpr -> TRExpr
rewriteTailCalls name (Loc.Located sp (Ty.Typed t expr)) = case expr of
  TRApp (Loc.Located _ (Ty.Typed _ (TRVar vb))) args
    | Res.symName vb == name ->
      Loc.Located sp (Ty.Typed t (TRRecur (map tcoExpr args)))
  TRIf c th el ->
    Loc.Located sp (Ty.Typed t (TRIf (tcoExpr c)
      (rewriteTailCalls name th) (rewriteTailCalls name el)))
  TRLet binds body ->
    let shadowed = any (\(n, _, _) -> n == name) binds
        body' = if shadowed then tcoExpr body else rewriteTailCalls name body
    in Loc.Located sp (Ty.Typed t (TRLet (map tcoBind binds) body'))
  TRCase scr arms ->
    Loc.Located sp (Ty.Typed t (TRCase (tcoExpr scr)
      [(p, rewriteTailCalls name b) | (p, b) <- arms]))
  _ -> Loc.Located sp (Ty.Typed t (tcoExprF expr))
