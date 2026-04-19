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

-- ENTRY POINT

typecheck :: Context -> Res.ResolvedCST -> Either [TypeError] TResolvedCST
typecheck importedCtx exprs =
  let typeDecls = extractTypeDecls exprs
      ctorCtx = buildCtorContext typeDecls
      builtInCtx = M.map (uncurry Forall) BuiltIn.builtInSchemes
      initialCtx = M.unions [importedCtx, ctorCtx, builtInCtx]
      fieldMap = buildFieldMap typeDecls
      env = InferEnv initialCtx fieldMap M.empty
      (typed, _, (constraints, inferErrs)) = RWS.runRWS (traverse infer exprs) env 0
  in case solveAll constraints of
    Left solveErrs -> Left (inferErrs ++ solveErrs)
    Right subst
      | null inferErrs -> Right (apply subst typed)
      | otherwise      -> Left inferErrs

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

data InferEnv = InferEnv
  { ieCtx    :: !Context
  , ieFields :: !FieldMap
  , ieFuncs  :: !FuncInfoMap
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
  tvs _                = S.empty

  apply s t@(Ty.TyVar tv)   = M.findWithDefault t tv s
  apply s (Ty.TyFun ats rt) = Ty.TyFun (map (apply s) ats) (apply s rt)
  apply s (Ty.TyCon sym ts) = Ty.TyCon sym $ map (apply s) ts
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

compose :: Subst -> Subst -> Subst
compose a b = M.map (apply a) (b `M.union` a)

-- CONTEXT BUILDING

extractTypeDecls :: [Res.RExpr] -> [(CST.Symbol, [CST.Symbol], [CST.DataCon])]
extractTypeDecls = foldr go []
  where
    go (Loc.Located _ (Res.RType name params ctors)) acc = (name, params, ctors) : acc
    go _ acc = acc

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
      _ -> ty

buildFieldMap :: [(CST.Symbol, [CST.Symbol], [CST.DataCon])] -> FieldMap
buildFieldMap = M.fromList . concatMap go
  where
    go (_typeName, _params, ctors) = concatMap ctorFields ctors
    ctorFields (CST.DataCon ctorName args (Just fieldNames)) =
      [(fname, (ctorName, idx, length args))
      | (idx, fname) <- zip [0..] fieldNames]
    ctorFields _ = []

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
    ats <- case funcInfo of
      Just (FuncInfo reqArity (FIOpt defaults)) -> do
        -- Fill in missing optional args with defaults
        let nPos = length posAts
            totalArity = reqArity + length defaults
        if nPos >= totalArity then pure posAts
        else if nPos < reqArity then pure posAts  -- too few, unification will error
        else do
          let nMissing = totalArity - nPos
              missingDefaults = drop (nPos - reqArity) defaults
          defaultAts <- traverse infer (take nMissing missingDefaults)
          pure (posAts ++ defaultAts)

      Just (FuncInfo reqArity FIRest) -> do
        -- Always pack args beyond reqArity into a Cons/Nil list
        let nPos = length posAts
        if nPos < reqArity then pure posAts  -- too few, unification will error
        else do
          let reqArgs  = take reqArity posAts
              restArgs = drop reqArity posAts
          elemTy <- if null restArgs then fresh
                    else pure (typeOf (head restArgs))
          listExpr <- buildListExpr sp elemTy restArgs
          pure (reqArgs ++ [listExpr])

      Just (FuncInfo reqArity (FIKey keyNames defaults)) -> do
        -- Reorder keyword args to match definition order
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
        -- No metadata or plain function — handle any inline key args
        if null keyExprs then pure posAts
        else do
          keyAts <- traverse (\(_, e) -> infer e) keyExprs
          pure (posAts ++ keyAts)

    rt <- fresh
    constrain sp (typeOf ft) (Ty.TyFun (map typeOf ats) rt)
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
  Res.RType name params ctors ->
    -- Constructors are registered in context by typecheck, just pass through here
    pure $ Ty.Typed (Ty.TyCon name []) (TRType name params ctors)
  Res.RFieldAccess fieldName subExpr -> do
    scrutExpr <- infer subExpr
    let scrutTy = typeOf scrutExpr
    fmap <- ieFields <$> RWS.ask
    case M.lookup fieldName fmap of
      Nothing -> do
        t <- recordError sp ("no field '" ++ T.unpack fieldName ++ "'")
        pure $ Ty.Typed t TRUnit
      Just (ctorName, fieldIdx, numFields) -> do
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
