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

-- ENTRY POINT

typecheck :: Res.ResolvedCST -> Either TypeError TResolvedCST
typecheck exprs = do
  let typeDecls = extractTypeDecls exprs
      ctorCtx = buildCtorContext typeDecls
      builtInCtx = M.map (uncurry Forall) BuiltIn.builtInSchemes
      initialCtx = M.union ctorCtx builtInCtx
  (typed, _, constraints) <- RWS.runRWST (traverse infer exprs) initialCtx 0
  subst <- solve constraints
  Right (apply subst typed)

-- Extract TYPE declarations from resolved expressions
extractTypeDecls :: [Res.RExpr] -> [(CST.Symbol, [CST.Symbol], [CST.DataCon])]
extractTypeDecls = foldr go []
  where
    go (Loc.Located _ (Res.RType name params ctors)) acc = (name, params, ctors) : acc
    go _ acc = acc

-- Build context with constructor types
buildCtorContext :: [(CST.Symbol, [CST.Symbol], [CST.DataCon])] -> Context
buildCtorContext = M.fromList . concatMap buildCtors
  where
    buildCtors (typeName, params, ctors) =
      let -- Map param names to TyVar indices: a -> 0, b -> 1, etc.
          paramMap = M.fromList (zip params [0..])
          paramTyVars = map Ty.TyVar [0 .. fromIntegral (length params - 1)]
          resultTy = Ty.TyCon typeName paramTyVars
          paramSet = S.fromList [0 .. fromIntegral (length params - 1)]
      in map (buildCtor paramMap paramSet resultTy) ctors

    buildCtor paramMap paramSet resultTy (CST.DataCon ctorName args) =
      let argTys = map (resolveTypeParams paramMap) args
          ctorTy = if null argTys
                   then resultTy
                   else Ty.TyFun argTys resultTy
          scheme = Forall paramSet ctorTy
      in (ctorName, scheme)

    -- Convert TyCon "A" [] to TyVar if "A" is a type parameter
    resolveTypeParams paramMap ty = case ty of
      Ty.TyCon name [] -> case M.lookup name paramMap of
        Just idx -> Ty.TyVar idx
        Nothing  -> ty
      Ty.TyCon name args -> Ty.TyCon name (map (resolveTypeParams paramMap) args)
      Ty.TyFun args ret -> Ty.TyFun (map (resolveTypeParams paramMap) args) (resolveTypeParams paramMap ret)
      _ -> ty

-- CORE

type TResolvedCST = [TRExpr]

type TRExpr = Loc.Located (Ty.Typed TRExprF)

data TRExprF
  = TRLit  CST.Literal
  | TRBool Bool
  | TRVar  Res.VarBinding
  | TRLam  [(CST.Symbol, Ty.Type)] Ty.Type TRExpr
  | TRLet  [(CST.Symbol, Ty.Type, TRExpr)] TRExpr
  | TRIf   TRExpr TRExpr TRExpr
  | TRApp  TRExpr [TRExpr]
  | TRType CST.Symbol [CST.Symbol] [CST.DataCon]
  deriving (Eq, Show)

data TypeError = TypeError
  { teSpan :: Loc.Span
  , teMsg  :: String
  }

infer :: Res.RExpr -> Infer TRExpr
infer (Loc.Located sp expr) = Loc.Located sp <$> case expr of
  Res.RLit l@(CST.LitInt _) -> do
    pure $ Ty.Typed Ty.TyInt (TRLit l)
  Res.RLit l@(CST.LitFlt _) -> do
    pure $ Ty.Typed Ty.TyFlt (TRLit l)
  Res.RLit l@(CST.LitStr _) -> do
    pure $ Ty.Typed Ty.TyStr (TRLit l)
  Res.RBool b -> do
    pure $ Ty.Typed Ty.TyBool (TRBool b)
  Res.RVar vb -> do
    ctx <- RWS.ask
    case M.lookup (Res.symName vb) ctx of
      Just scheme -> do
        t <- instantiate scheme
        pure $ Ty.Typed t (TRVar vb)
      Nothing -> throwTE sp ("Undefined variable " ++ show (Res.symName vb))
  Res.RIf c t e -> do
    ct <- infer c
    tt <- infer t
    et <- infer e
    constrain (typeOf ct) Ty.TyBool
    constrain (typeOf tt) (typeOf et)
    pure $ Ty.Typed (typeOf tt) (TRIf ct tt et)
  Res.RApp fexpr aexprs -> do
    ft <- infer fexpr
    ats <- traverse infer aexprs
    rt <- fresh
    constrain (typeOf ft) (Ty.TyFun (map typeOf ats) rt)
    pure $ Ty.Typed rt (TRApp ft ats)
  Res.RLam params _mRet body -> do
    paramTys <- traverse paramType params
    let paramNames = map CST.symName params
        paramSchemes = map (Forall S.empty) paramTys
        newBindings = M.fromList (zip paramNames paramSchemes)
    bodyExpr <- RWS.local (M.union newBindings) (infer body)
    let funTy = Ty.TyFun paramTys (typeOf bodyExpr)
    pure $ Ty.Typed funTy (TRLam (zip paramNames paramTys) (typeOf bodyExpr) bodyExpr)
  Res.RLet binds body -> do
    ctx <- RWS.ask
    let names = map (CST.symName . fst) binds
    freshTys <- traverse (const fresh) names
    let monoSchemes = map (Forall S.empty) freshTys
        recCtx = M.union (M.fromList (zip names monoSchemes)) ctx
    rhsExprs <- traverse (\(_, rhs) -> RWS.local (const recCtx) (infer rhs)) binds
    sequence_ $ zipWith constrain freshTys (map typeOf rhsExprs)
    let generalizedSchemes = map (generalize ctx . typeOf) rhsExprs
        bodyCtx = M.union (M.fromList (zip names generalizedSchemes)) ctx
    bodyExpr <- RWS.local (const bodyCtx) (infer body)
    let typedBinds = zip3 names (map typeOf rhsExprs) rhsExprs
    pure $ Ty.Typed (typeOf bodyExpr) (TRLet typedBinds bodyExpr)
  Res.RType name params ctors -> do
    -- Constructors are registered in context by typecheck, just pass through here
    pure $ Ty.Typed (Ty.TyCon name []) (TRType name params ctors)

unify :: Ty.Type -> Ty.Type -> Solve Subst
unify (Ty.TyVar v) t = bind v t
unify t (Ty.TyVar v) = bind v t
unify (Ty.TyFun as1 r1) (Ty.TyFun as2 r2) = unifyMany (r1:as1) (r2:as2)
unify (Ty.TyCon n1 ts1) (Ty.TyCon n2 ts2)
  | n1 == n2  = unifyMany ts1 ts2
unify Ty.TyInt Ty.TyInt = Right M.empty
unify Ty.TyFlt Ty.TyFlt = Right M.empty
unify Ty.TyStr Ty.TyStr = Right M.empty
unify Ty.TyBool Ty.TyBool = Right M.empty
unify t1 t2 = Left $ solveErr ("cannot unify " ++ show t1 ++ " with " ++ show t2)

unifyMany :: [Ty.Type] -> [Ty.Type] -> Solve Subst
unifyMany [] [] = Right M.empty
unifyMany (t1:ts1) (t2:ts2) = do
  s1 <- unify t1 t2
  s2 <- unifyMany (apply s1 ts1) (apply s1 ts2)
  Right (compose s2 s1)
unifyMany _ _ = Left $ solveErr "type mismatch: different arities"

bind :: TyVar -> Ty.Type -> Solve Subst
bind tv t
  | tv `S.member` tvs t = Left $ solveErr ("infinite type " ++ show tv ++ " ~ " ++ show t)
  | otherwise = Right $ M.singleton tv t

solveErr :: String -> TypeError
solveErr msg = TypeError (Loc.Span p p) msg
  where p = Loc.Pos "" 0 0

solve :: Constraints -> Solve Subst
solve [] = Right M.empty
solve (Constraint t1 t2 : cs) = do
  s1 <- unify t1 t2
  s2 <- solve (apply s1 cs)
  Right (compose s2 s1)

-- HELPERS

type Solve a = Either TypeError a

data Scheme = Forall (S.Set TyVar) Ty.Type

data Constraint = Constraint Ty.Type Ty.Type

type Context = M.Map CST.Symbol Scheme

type Constraints = [Constraint]

type TyVar = Integer

type Subst = M.Map TyVar Ty.Type

type Infer a = RWS.RWST Context Constraints TyVar (Either TypeError) a

throwTE :: Loc.Span -> String -> Infer a
throwTE sp msg = RWS.lift . Left $ TypeError sp msg

paramType :: CST.TSymbol -> Infer Ty.Type
paramType tsym = case CST.symType tsym of
  Just t  -> pure t
  Nothing -> fresh

typeOf :: TRExpr -> Ty.Type
typeOf (Loc.Located _ (Ty.Typed t _)) = t

fresh :: Infer Ty.Type
fresh = RWS.state (\n -> (Ty.TyVar n, n+1))

constrain :: Ty.Type -> Ty.Type -> Infer ()
constrain t1 t2 = RWS.tell [Constraint t1 t2]

compose :: Subst -> Subst -> Subst
compose a b = M.map (apply a) (b `M.union` a)

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
  tvs (Constraint t1 t2) = tvs t1 `S.union` tvs t2
  apply s (Constraint t1 t2) = Constraint (apply s t1) (apply s t2)

instance Substitutable a => Substitutable [a] where
  tvs l = foldr (S.union . tvs) S.empty l
  apply s = map (apply s)

instance Substitutable a => Substitutable (Loc.Located a) where
  tvs (Loc.Located _ a) = tvs a
  apply s (Loc.Located sp a) = Loc.Located sp (apply s a)

instance Substitutable a => Substitutable (Ty.Typed a) where
  tvs (Ty.Typed t a) = tvs t `S.union` tvs a
  apply s (Ty.Typed t a) = Ty.Typed (apply s t) (apply s a)

instance Substitutable TRExprF where
  tvs (TRLit _) = S.empty
  tvs (TRBool _) = S.empty
  tvs (TRVar _) = S.empty
  tvs (TRLam params retTy body) = tvs (map snd params) `S.union` tvs retTy `S.union` tvs body
  tvs (TRLet binds body) = foldr S.union S.empty [tvs t `S.union` tvs e | (_, t, e) <- binds] `S.union` tvs body
  tvs (TRIf c t e) = tvs c `S.union` tvs t `S.union` tvs e
  tvs (TRApp f as) = tvs f `S.union` tvs as
  tvs (TRType _ _ _) = S.empty

  apply _ (TRLit l) = TRLit l
  apply _ (TRBool b) = TRBool b
  apply _ (TRVar v) = TRVar v
  apply s (TRLam params retTy body) = TRLam [(n, apply s t) | (n, t) <- params] (apply s retTy) (apply s body)
  apply s (TRLet binds body) = TRLet [(n, apply s t, apply s e) | (n, t, e) <- binds] (apply s body)
  apply s (TRIf c t e) = TRIf (apply s c) (apply s t) (apply s e)
  apply s (TRApp f as) = TRApp (apply s f) (apply s as)
  apply _ (TRType n ps cs) = TRType n ps cs

generalize :: Context -> Ty.Type -> Scheme
generalize ctx t = Forall (tvs t `S.difference` tvs (M.elems ctx)) t

instantiate :: Scheme -> Infer Ty.Type
instantiate (Forall vs t) = do
  let vars = S.toList vs
  ftvs <- traverse (const fresh) vars
  let subst = M.fromList (zip vars ftvs)
  return $ apply subst t
