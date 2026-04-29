{-# LANGUAGE OverloadedStrings #-}

module Pllisp.MacroExpand
  ( CompileState(..)
  , ModuleResult(..)
  , defaultState
  , primitiveState
  , finalizeModuleState
  , mergeCompileStates
  , expand
  , expandWith
  , expandModuleWith
  , extractMacroDefs
  ) where

import Control.Monad (foldM)
import qualified Control.Monad.State.Strict as State
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import System.IO.Unsafe (unsafePerformIO)

import qualified Pllisp.CST as CST
import qualified Pllisp.Module as Mod
import qualified Pllisp.MacroInterp as MI
import qualified Pllisp.Resolve as Resolve
import qualified Pllisp.SExpr as SExpr
import qualified Pllisp.SrcLoc as Loc
import qualified Pllisp.Stdlib as Stdlib
import qualified Pllisp.Type as Ty
import qualified Pllisp.TypeCheck as TC

data MacroClause = MacroClause
  { mcParams :: [MacroParam]
  , mcTypedBody :: TC.TRExpr
  , mcEnv :: MI.Env
  } deriving (Show)

data MacroParam
  = ParamSingle T.Text
  | ParamRest T.Text
  | ParamDestructure [MacroParam]
  deriving (Show)

type MacroTable = M.Map T.Text MacroClause
type Bindings = M.Map T.Text SExpr.SExpr

data CompileState = CompileState
  { csEnv :: MI.Env
  , csMacros :: MacroTable
  , csCtCtx :: TC.Context
  , csCtEnvs :: TC.TCEnvs
  , csRtCtx :: TC.Context
  , csRtEnvs :: TC.TCEnvs
  , csEnvOrigins :: M.Map T.Text T.Text
  , csMacroOrigins :: M.Map T.Text T.Text
  } deriving (Show)

data ModuleResult = ModuleResult
  { mrExpanded :: [SExpr.SExpr]
  , mrState :: CompileState
  } deriving (Show)

type ExpandM = State.StateT Int (Either String)

data EvalPhase
  = PhaseCompileTop
  | PhaseLoadTop
  | PhaseExecute
  deriving (Eq)

primitiveOrigin :: T.Text
primitiveOrigin = "%PRIMITIVE"

compileTimeBuiltins :: TC.Context
compileTimeBuiltins = M.fromList
  [ ("SYNTAX-CAR", syntaxFun [Ty.TySyntax] Ty.TySyntax)
  , ("SYNTAX-CDR", syntaxFun [Ty.TySyntax] Ty.TySyntax)
  , ("SYNTAX-LENGTH", syntaxFun [Ty.TySyntax] Ty.TyInt)
  , ("SYNTAX-EQUAL?", syntaxFun [Ty.TySyntax, Ty.TySyntax] Ty.TyBool)
  , ("SYNTAX-NULL?", syntaxFun [Ty.TySyntax] Ty.TyBool)
  , ("SYNTAX-SYMBOL?", syntaxFun [Ty.TySyntax] Ty.TyBool)
  , ("SYNTAX-LIST?", syntaxFun [Ty.TySyntax] Ty.TyBool)
  , ("SYNTAX-STRING?", syntaxFun [Ty.TySyntax] Ty.TyBool)
  , ("SYNTAX-NUMBER?", syntaxFun [Ty.TySyntax] Ty.TyBool)
  , ("SYNTAX-BOOL?", syntaxFun [Ty.TySyntax] Ty.TyBool)
  , ("SYNTAX-TYPE?", syntaxFun [Ty.TySyntax] Ty.TyBool)
  , ("EQ", TC.Forall (S.singleton 0) (Ty.TyFun [Ty.TyVar 0, Ty.TyVar 0] Ty.TyBool))
  , ("ERROR", TC.Forall (S.singleton 0) (Ty.TyFun [Ty.TyStr] (Ty.TyVar 0)))
  , ("SYNTAX-LIFT", TC.Forall (S.singleton 0) (Ty.TyFun [Ty.TyVar 0] Ty.TySyntax))
  , ("SYNTAX-SYMBOL", syntaxFun [Ty.TyStr] Ty.TySyntax)
  , ("SYNTAX-INT", syntaxFun [Ty.TyInt] Ty.TySyntax)
  , ("SYNTAX-FLOAT", syntaxFun [Ty.TyFlt] Ty.TySyntax)
  , ("SYNTAX-STRING", syntaxFun [Ty.TyStr] Ty.TySyntax)
  , ("SYNTAX-BOOL", syntaxFun [Ty.TyBool] Ty.TySyntax)
  , ("SYNTAX-USYM", syntaxFun [Ty.TyStr] Ty.TySyntax)
  , ("SYNTAX-RX", syntaxFun [Ty.TyStr, Ty.TyStr] Ty.TySyntax)
  , ("SYNTAX-TYPE", syntaxFun [Ty.TySyntax] Ty.TySyntax)
  , ("SYNTAX-EMPTY", TC.Forall S.empty Ty.TySyntax)
  , ("SYNTAX-CONS", syntaxFun [Ty.TySyntax, Ty.TySyntax] Ty.TySyntax)
  , ("SYNTAX-APPEND", syntaxFun [Ty.TySyntax, Ty.TySyntax] Ty.TySyntax)
  , ("SYNTAX-INT-VALUE", syntaxFun [Ty.TySyntax] Ty.TyInt)
  , ("SYNTAX-FLOAT-VALUE", syntaxFun [Ty.TySyntax] Ty.TyFlt)
  , ("SYNTAX-STRING-VALUE", syntaxFun [Ty.TySyntax] Ty.TyStr)
  , ("SYNTAX-SYMBOL-NAME", syntaxFun [Ty.TySyntax] Ty.TyStr)
  , ("SYNTAX-USYM-NAME", syntaxFun [Ty.TySyntax] Ty.TyStr)
  , ("RX-COMPILE", syntaxFun [Ty.TyStr] Ty.TyRx)
  , ("RX-MATCH", syntaxFun [Ty.TyRx, Ty.TyStr] Ty.TyBool)
  , ("RX-FIND", syntaxFun [Ty.TyRx, Ty.TyStr] Ty.TyStr)
  , ("RX-SUB", syntaxFun [Ty.TyRx, Ty.TyStr, Ty.TyStr] Ty.TyStr)
  , ("RX-GSUB", syntaxFun [Ty.TyRx, Ty.TyStr, Ty.TyStr] Ty.TyStr)
  , ("RX-SPLIT", syntaxFun [Ty.TyRx, Ty.TyStr] (Ty.TyCon "LIST" [Ty.TyStr]))
  , ("RX-CAPTURES", syntaxFun [Ty.TyRx, Ty.TyStr] (Ty.TyCon "LIST" [Ty.TyStr]))
  ]
  where
    syntaxFun args ret = TC.Forall S.empty (Ty.TyFun args ret)

primitiveState :: CompileState
primitiveState =
  CompileState
    { csEnv = MI.defaultEnv
    , csMacros = M.empty
    , csCtCtx = compileTimeBuiltins
    , csCtEnvs = TC.emptyTCEnvs
    , csRtCtx = M.empty
    , csRtEnvs = TC.emptyTCEnvs
    , csEnvOrigins = M.fromList [(name, primitiveOrigin) | name <- M.keys MI.defaultEnv]
    , csMacroOrigins = M.empty
    }

defaultState :: CompileState
defaultState = unsafePerformIO loadDefaultState
{-# NOINLINE defaultState #-}

loadDefaultState :: IO CompileState
loadDefaultState = do
  prelude <- Stdlib.loadPrelude
  case expandModuleWith "PRELUDE" primitiveState prelude of
    Left err -> error ("BUG: failed to build compile-time PRELUDE: " ++ err)
    Right result ->
      case finalizeModuleState "PRELUDE" (mrState result) (mrExpanded result) of
        Left err -> error ("BUG: failed to build runtime PRELUDE compile-time surface: " ++ err)
        Right st -> pure st

emptyState :: CompileState
emptyState =
  CompileState
    { csEnv = M.empty
    , csMacros = M.empty
    , csCtCtx = M.empty
    , csCtEnvs = TC.emptyTCEnvs
    , csRtCtx = M.empty
    , csRtEnvs = TC.emptyTCEnvs
    , csEnvOrigins = M.empty
    , csMacroOrigins = M.empty
    }

finalizeModuleState :: T.Text -> CompileState -> [SExpr.SExpr] -> Either String CompileState
finalizeModuleState modName st expanded = do
  prog <- firstLeft (("syntax error: " ++) . SExpr.ceMsg) (SExpr.toProgram expanded)
  exprs <- firstLeft ("desugar error: " ++) (Mod.desugarTopLevel (CST.progExprs prog))
  let resolveScope = M.keysSet (csRtCtx st)
  resolved <- firstLeft renderResolveErrs (Resolve.resolveWith resolveScope M.empty exprs)
  (typed, fullEnvs) <- firstLeft renderTypeErrs (TC.typecheckWith (csRtEnvs st) (csRtCtx st) resolved)
  let exports = collectSurfaceExports fullEnvs typed
      declExports = collectDeclarationExports fullEnvs typed
      visibleRuntimeNames = S.fromList (concatMap compileVisibleRuntimeBindingNames expanded)
      shouldStrip name =
        S.member name visibleRuntimeNames
          && M.lookup name (csEnvOrigins st) /= Just primitiveOrigin
      replayBase =
        st
          { csEnv = M.filterWithKey (\name _ -> not (shouldStrip name)) (csEnv st)
          , csCtCtx = M.filterWithKey (\name _ -> not (shouldStrip name)) (csCtCtx st)
          , csEnvOrigins = M.filterWithKey (\name _ -> not (shouldStrip name)) (csEnvOrigins st)
          }
  rebuilt <- State.evalStateT (foldM replayRuntimeSurface replayBase expanded) 0
  let localExports = M.difference exports (csRtCtx st)
      originNames = S.toList visibleRuntimeNames ++ M.keys localExports
      origins' = M.union (M.fromList [(name, modName) | name <- originNames]) (csEnvOrigins rebuilt)
  pure rebuilt
    { csCtCtx = M.union declExports (csCtCtx rebuilt)
    , csCtEnvs = TC.mergeTCEnvs (csCtEnvs st) fullEnvs
    , csRtCtx = M.union exports (csRtCtx st)
    , csRtEnvs = fullEnvs
    , csEnvOrigins = origins'
    }
  where
    renderResolveErrs errs = unlines (map Resolve.errMsg errs)
    renderTypeErrs errs = unlines (map TC.teMsg errs)
    replayRuntimeSurface cur sx
      | isCompileVisibleRuntimeForm sx = registerRuntimeDeclForm modName cur sx
      | otherwise = pure cur

firstLeft :: (e -> e') -> Either e a -> Either e' a
firstLeft f = either (Left . f) Right

mergeVisibleEnv :: MI.Env -> MI.Env -> M.Map T.Text a -> MI.Env
mergeVisibleEnv base full visible =
  foldl insertVisible base (M.keys visible)
  where
    insertVisible env name =
      case M.lookup name full of
        Just val -> M.insert name val env
        Nothing -> env

collectSurfaceExports :: TC.TCEnvs -> TC.TResolvedCST -> TC.Context
collectSurfaceExports envs typed =
  M.unions
    [ Mod.collectExports envs typed
    , collectDeclarationExports envs typed
    ]

collectDeclarationExports :: TC.TCEnvs -> TC.TResolvedCST -> TC.Context
collectDeclarationExports envs typed =
  M.unions
    [ ctorExports
    , TC.methodSchemes envs
    , TC.buildFFIContext ffiDecls
    , TC.buildFFIContext ffiVarDecls
    , TC.buildFFIStructCtorContext ffiStructDecls
    , ffiEnumCtx
    , TC.buildFFIContext ffiCallbackDecls
    ]
  where
    typeDecls =
      [ (name, params, ctors)
      | Loc.Located _ (Ty.Typed _ (TC.TRType name params ctors)) <- typed
      ]
    ctorExports = TC.buildCtorContext typeDecls
    ffiDecls =
      [ (name, paramTys, retTy)
      | Loc.Located _ (Ty.Typed _ (TC.TRFFI name paramTys retTy)) <- typed
      ]
    ffiVarDecls =
      [ (name, paramTys, retTy)
      | Loc.Located _ (Ty.Typed _ (TC.TRFFIVar name paramTys retTy)) <- typed
      ]
    ffiStructDecls =
      [ (name, fields)
      | Loc.Located _ (Ty.Typed _ (TC.TRFFIStruct name fields)) <- typed
      ]
    ffiEnumCtx = M.fromList
      [ (name, TC.Forall S.empty Ty.TyInt)
      | Loc.Located _ (Ty.Typed _ (TC.TRFFIEnum _ variants)) <- typed
      , (name, _) <- variants
      ]
    ffiCallbackDecls =
      [ (name, paramTys, retTy)
      | Loc.Located _ (Ty.Typed _ (TC.TRFFICallback name paramTys retTy)) <- typed
      ]

throwExpandError :: String -> ExpandM a
throwExpandError = State.StateT . const . Left

liftEither :: Either String a -> ExpandM a
liftEither = either throwExpandError pure

maxDepth :: Int
maxDepth = 256

expand :: [SExpr.SExpr] -> Either String [SExpr.SExpr]
expand = expandWith defaultState

expandWith :: CompileState -> [SExpr.SExpr] -> Either String [SExpr.SExpr]
expandWith base sexprs =
  mrExpanded <$> expandModuleWith (moduleNameOrUser sexprs) base sexprs

expandModuleWith :: T.Text -> CompileState -> [SExpr.SExpr] -> Either String ModuleResult
expandModuleWith modName base sexprs =
  State.evalStateT (go base [] sexprs) 0
  where
    go st out [] = pure $ ModuleResult (reverse out) st
    go st out (sx : rest) = do
      (st', emitted) <- processTopLevel modName st sx
      go st' (reverse emitted ++ out) rest

mergeCompileStates :: [CompileState] -> Either String CompileState
mergeCompileStates = foldM mergeOne emptyState
  where
    mergeOne acc st = do
      acc1 <- foldM mergeEnvEntry acc (M.toList (csEnv st))
      let acc2 = acc1
            { csCtCtx = M.union (csCtCtx acc1) (csCtCtx st)
            , csCtEnvs = TC.mergeTCEnvs (csCtEnvs acc1) (csCtEnvs st)
            , csRtCtx = M.union (csRtCtx acc1) (csRtCtx st)
            , csRtEnvs = TC.mergeTCEnvs (csRtEnvs acc1) (csRtEnvs st)
            }
      foldM mergeMacroEntry acc2 (M.toList (csMacros st))

      where
        mergeEnvEntry cur (name, val) =
          case M.lookup name (csEnvOrigins st) of
            Nothing -> pure cur
            Just origin ->
              case M.lookup name (csEnvOrigins cur) of
                Nothing -> pure
                  (cur
                    { csEnv = M.insert name val (csEnv cur)
                    , csEnvOrigins = M.insert name origin (csEnvOrigins cur)
                    })
                Just existingOrigin
                  | existingOrigin == origin -> pure cur
                  | otherwise ->
                      Left ("compile-time helper collision: "
                        ++ T.unpack name
                        ++ " from "
                        ++ T.unpack existingOrigin
                        ++ " and "
                        ++ T.unpack origin)

        mergeMacroEntry cur (name, clause) =
          case M.lookup name (csMacroOrigins st) of
            Nothing -> pure cur
            Just origin ->
              case M.lookup name (csMacroOrigins cur) of
                Nothing -> pure
                  (cur
                    { csMacros = M.insert name clause (csMacros cur)
                    , csMacroOrigins = M.insert name origin (csMacroOrigins cur)
                    })
                Just existingOrigin
                  | existingOrigin == origin -> pure cur
                  | otherwise ->
                      Left ("duplicate imported macro definition: "
                        ++ T.unpack name
                        ++ " from "
                        ++ T.unpack existingOrigin
                        ++ " and "
                        ++ T.unpack origin)

processTopLevel :: T.Text -> CompileState -> SExpr.SExpr -> ExpandM (CompileState, [SExpr.SExpr])
processTopLevel modName st sx = case Loc.locVal sx of
  SExpr.SList (Loc.Located _ (SExpr.SAtom "MAC") : macRest) -> do
    st' <- defineMacro modName st macRest
    pure (st', [])
  SExpr.SList (Loc.Located _ (SExpr.SAtom "EVAL-WHEN") : phaseSx : bodyForms) ->
    processEvalWhen modName st phaseSx bodyForms
  _ -> do
    expanded <- expandExpr (csMacros st) 0 sx
    st' <- if isCompileVisibleRuntimeForm expanded
      then registerRuntimeDeclForm modName st expanded
      else pure st
    pure (st', [expanded])

processEvalWhen :: T.Text -> CompileState -> SExpr.SExpr -> [SExpr.SExpr] -> ExpandM (CompileState, [SExpr.SExpr])
processEvalWhen modName st phaseSx bodyForms = do
  phases <- liftEither (parseEvalPhases phaseSx)
  let doCompile = PhaseCompileTop `elem` phases
      doEmit = PhaseLoadTop `elem` phases || PhaseExecute `elem` phases
  go st [] doCompile doEmit bodyForms
  where
    go cur out _ _ [] = pure (cur, reverse out)
    go cur out doCompile doEmit (form : rest) = do
      (cur', emitted) <- processEvalWhenBody modName doCompile doEmit cur form
      go cur' (reverse emitted ++ out) doCompile doEmit rest

processEvalWhenBody :: T.Text -> Bool -> Bool -> CompileState -> SExpr.SExpr -> ExpandM (CompileState, [SExpr.SExpr])
processEvalWhenBody modName doCompile doEmit st sx = case Loc.locVal sx of
  SExpr.SList (Loc.Located _ (SExpr.SAtom "MAC") : macRest)
    | doCompile -> do
        st' <- defineMacro modName st macRest
        pure (st', [])
    | otherwise -> pure (st, [])
  SExpr.SList (Loc.Located _ (SExpr.SAtom "EVAL-WHEN") : phaseSx : bodyForms) ->
    processEvalWhen modName st phaseSx bodyForms
  _ -> do
    expanded <- expandExpr (csMacros st) 0 sx
    st' <- if doCompile then compileTopLevelForm modName st expanded else pure st
    st'' <- if doEmit && not doCompile && isCompileVisibleRuntimeForm expanded
      then registerRuntimeDeclForm modName st' expanded
      else pure st'
    pure (st'', if doEmit then [expanded] else [])

compileTopLevelForm :: T.Text -> CompileState -> SExpr.SExpr -> ExpandM CompileState
compileTopLevelForm modName st sx = do
  expr <- liftEither (firstLeft SExpr.ceMsg (SExpr.toCompileExpr sx))
  let resolveScope = M.keysSet (csCtCtx st)
  resolved <- liftEither (firstLeft renderResolveErrs (Resolve.resolveWith resolveScope M.empty [expr]))
  (typed, fullEnvs) <- liftEither (firstLeft renderTypeErrs (TC.typecheckWith (csCtEnvs st) (csCtCtx st) resolved))
  env' <- liftEither (firstLeft id (MI.runInterpM (MI.loadTypedTopLevelBindings (csEnv st) typed)))
  let exports = collectSurfaceExports fullEnvs typed
      localExports = M.difference exports (csCtCtx st)
      names = M.keys localExports
      origins' = foldl (\acc name -> M.insert name modName acc) (csEnvOrigins st) names
  pure st
    { csEnv = env'
    , csCtCtx = M.union exports (csCtCtx st)
    , csCtEnvs = fullEnvs
    , csEnvOrigins = origins'
    }
  where
    renderResolveErrs errs = unlines (map Resolve.errMsg errs)
    renderTypeErrs errs = unlines (map TC.teMsg errs)

registerRuntimeDeclForm :: T.Text -> CompileState -> SExpr.SExpr -> ExpandM CompileState
registerRuntimeDeclForm modName st sx = do
  expr <- liftEither (firstLeft SExpr.ceMsg (SExpr.toExpr sx))
  let resolveScope = M.keysSet (csRtCtx st)
  resolved <- liftEither (firstLeft renderResolveErrs (Resolve.resolveWith resolveScope M.empty [expr]))
  (typed, fullEnvs) <- liftEither (firstLeft renderTypeErrs (TC.typecheckWith (csRtEnvs st) (csRtCtx st) resolved))
  let exports = collectSurfaceExports fullEnvs typed
      rtState =
        st
          { csRtCtx = M.union exports (csRtCtx st)
          , csRtEnvs = fullEnvs
          }
  case typed of
    [Loc.Located _ (Ty.Typed _ (TC.TRLet binds _))]
      | isCompileVisibleRuntimeLet sx -> do
          env' <- registerRuntimeBindings st binds
          let env'' = sanitizeVisibleBindings st exports env'
          let localExports = M.difference exports (csRtCtx st)
              names = M.keys localExports
              origins' = foldl (\acc name -> M.insert name modName acc) (csEnvOrigins st) names
          pure rtState
            { csEnv = env''
            , csCtCtx = M.union exports (csCtCtx st)
            , csCtEnvs = TC.mergeTCEnvs (csCtEnvs st) fullEnvs
            , csEnvOrigins = origins'
            }
    _ -> case firstLeft id (MI.runInterpM (MI.loadTypedTopLevelForms (csEnv st) typed)) of
      Right env' -> do
        let ctEnv = sanitizeVisibleBindings st exports (mergeVisibleEnv (csEnv st) env' exports)
            localExports = M.difference exports (csRtCtx st)
            names = M.keys localExports
            origins' = foldl (\acc name -> M.insert name modName acc) (csEnvOrigins st) names
        pure rtState
          { csEnv = ctEnv
          , csCtCtx = M.union exports (csCtCtx st)
          , csCtEnvs = TC.mergeTCEnvs (csCtEnvs st) fullEnvs
          , csEnvOrigins = origins'
          }
      Left err
        | isCompileVisibleRuntimeLet sx ->
            pure rtState
        | otherwise ->
            liftEither (Left err)
  where
    renderResolveErrs errs = unlines (map Resolve.errMsg errs)
    renderTypeErrs errs = unlines (map TC.teMsg errs)

registerRuntimeBindings :: CompileState -> [(CST.Symbol, t, TC.TRExpr)] -> ExpandM MI.Env
registerRuntimeBindings st binds = go (csEnv st) M.empty binds
  where
    go env _ [] = pure env
    go env local ((name, _, rhs) : rest) =
      case analyzeCtExpr st local rhs of
        Left cause -> do
          let reason = unavailableReason name cause
              (env', local') = installUnavailable name reason env local
          go env' local' rest
        Right () ->
          case firstLeft id (MI.runInterpM (MI.evalTyped env rhs)) of
            Right val -> do
              case valueUnavailable st val of
                Just cause -> do
                  let reason = unavailableReason name cause
                      (env', local') = installUnavailable name reason env local
                  go env' local' rest
                Nothing -> do
                  let env' = M.insert name val env
                      local' = M.insert name Nothing local
                  go env' local' rest
            Left cause -> do
              let reason = unavailableReason name cause
                  (env', local') = installUnavailable name reason env local
              go env' local' rest

    unavailableReason name cause =
      compileTimeUnavailable name cause

    installUnavailable name reason env local =
      case primitiveFallback name of
        Just primVal ->
          (M.insert name primVal env, M.insert name Nothing local)
        _ ->
          ( M.insert name (MI.MUnavailable name reason) env
          , M.insert name (Just reason) local
          )

    primitiveFallback name = M.lookup name MI.defaultEnv

type LocalAvailability = M.Map T.Text (Maybe String)

sanitizeVisibleBindings :: CompileState -> TC.Context -> MI.Env -> MI.Env
sanitizeVisibleBindings st visible env =
  foldl sanitizeOne env (M.keys visible)
  where
    sanitizeOne acc name =
      case M.lookup name acc of
        Just val ->
          case valueUnavailable st val of
            Just cause ->
              case M.lookup name MI.defaultEnv of
                Just primVal -> M.insert name primVal acc
                Nothing ->
                  M.insert name (MI.MUnavailable name (compileTimeUnavailable name cause)) acc
            Nothing -> acc
        Nothing -> acc

compileTimeUnavailable :: T.Text -> String -> String
compileTimeUnavailable name cause =
  T.unpack name ++ " is not available at compile time"
    ++ if null cause then "" else ": " ++ cause

valueUnavailable :: CompileState -> MI.MVal -> Maybe String
valueUnavailable outerSt (MI.MTypedClosure closureEnv params body) =
  let closureSt =
        outerSt
          { csEnv = closureEnv
          , csRtCtx = M.empty
          }
      local' = foldl (\acc param -> M.insert param Nothing acc) M.empty params
  in either Just (const Nothing) (analyzeCtExpr closureSt local' body)
valueUnavailable _ _ = Nothing

analyzeCtExpr :: CompileState -> LocalAvailability -> TC.TRExpr -> Either String ()
analyzeCtExpr st local (Loc.Located _ (Ty.Typed _ expr)) = case expr of
  TC.TRLit _ -> Right ()
  TC.TRBool _ -> Right ()
  TC.TRUnit -> Right ()
  TC.TRVar vb ->
    analyzeCtName st local (Resolve.symName vb)
  TC.TRLam params _ body ->
    analyzeCtExpr st (foldl (\acc (name, _) -> M.insert name Nothing acc) local params) body
  TC.TRLet binds body -> do
    local' <- analyzeCtBinds st local binds
    analyzeCtExpr st local' body
  TC.TRIf cond thenBr elseBr -> do
    analyzeCtExpr st local cond
    analyzeCtExpr st local thenBr
    analyzeCtExpr st local elseBr
  TC.TRApp fn args -> do
    analyzeCtExpr st local fn
    mapM_ (analyzeCtExpr st local) args
  TC.TRType _ _ _ -> Right ()
  TC.TRCase scrutinee arms -> do
    analyzeCtExpr st local scrutinee
    mapM_ analyzeArm arms
  TC.TRLoop params body ->
    analyzeCtExpr st (foldl (\acc (name, _) -> M.insert name Nothing acc) local params) body
  TC.TRRecur args ->
    mapM_ (analyzeCtExpr st local) args
  TC.TRFFI name _ _ ->
    Left ("ffi not available at macro expansion time: " ++ T.unpack name)
  TC.TRFFIStruct _ _ ->
    Right ()
  TC.TRFFIVar name _ _ ->
    Left ("ffi not available at macro expansion time: " ++ T.unpack name)
  TC.TRFFIEnum _ _ ->
    Right ()
  TC.TRFFICallback name _ _ ->
    Left ("ffi not available at macro expansion time: " ++ T.unpack name)
  where
    analyzeArm (pat, body) =
      analyzeCtExpr st (addPatBindings local pat) body

analyzeCtBinds :: CompileState -> LocalAvailability -> [(CST.Symbol, t, TC.TRExpr)] -> Either String LocalAvailability
analyzeCtBinds _ local [] = Right local
analyzeCtBinds st local ((name, _, rhs) : rest) =
  let local' = case analyzeCtExpr st local rhs of
        Left cause -> M.insert name (Just (T.unpack name ++ " is not available at compile time: " ++ cause)) local
        Right () -> M.insert name Nothing local
  in analyzeCtBinds st local' rest

analyzeCtName :: CompileState -> LocalAvailability -> T.Text -> Either String ()
analyzeCtName st local name =
  case M.lookup name local of
    Just Nothing -> Right ()
    Just (Just reason) -> Left reason
    Nothing -> case M.lookup name (csEnv st) of
      Just MI.MUnavailable {} -> case M.lookup name (csEnv st) of
        Just (MI.MUnavailable _ reason) -> Left reason
        _ -> Right ()
      Just _ -> Right ()
      Nothing ->
        Left (T.unpack name ++ " is not available at compile time")

addPatBindings :: LocalAvailability -> TC.TRPattern -> LocalAvailability
addPatBindings local pat = case pat of
  TC.TRPatVar name _ -> M.insert name Nothing local
  TC.TRPatCon _ _ pats -> foldl addPatBindings local pats
  _ -> local

defineMacro :: T.Text -> CompileState -> [SExpr.SExpr] -> ExpandM CompileState
defineMacro modName st macRest =
  case parseMacDef macRest of
    Left err -> throwExpandError err
    Right (name, params, template)
      | M.member name (csMacros st) ->
          throwExpandError ("duplicate macro definition: " ++ T.unpack name)
      | otherwise ->
          case compileMacroBody st params template of
            Left compileErr ->
              throwExpandError ("typed macro compilation failed for " ++ T.unpack name ++ ": " ++ compileErr)
            Right typedBody ->
              pure st
                { csMacros = M.insert name (MacroClause params typedBody (csEnv st)) (csMacros st)
                , csMacroOrigins = M.insert name modName (csMacroOrigins st)
                }

parseMacDef :: [SExpr.SExpr] -> Either String (T.Text, [MacroParam], SExpr.SExpr)
parseMacDef [Loc.Located _ (SExpr.SAtom name), Loc.Located _ (SExpr.SList paramSxs), template] = do
  params <- parseParams paramSxs
  Right (name, params, template)
parseMacDef _ = Left "invalid mac definition"

parseParams :: [SExpr.SExpr] -> Either String [MacroParam]
parseParams [] = Right []
parseParams [Loc.Located _ (SExpr.SAtom "&REST"), Loc.Located _ (SExpr.SAtom name)] =
  Right [ParamRest name]
parseParams (Loc.Located _ (SExpr.SAtom "&REST") : _) =
  Left "&rest must be followed by exactly one name at end of param list"
parseParams (Loc.Located _ (SExpr.SAtom name) : rest) = do
  rest' <- parseParams rest
  Right (ParamSingle name : rest')
parseParams (Loc.Located _ (SExpr.SList subParams) : rest) = do
  sub <- parseParams subParams
  rest' <- parseParams rest
  Right (ParamDestructure sub : rest')
parseParams _ = Left "invalid macro parameter"

parseEvalPhases :: SExpr.SExpr -> Either String [EvalPhase]
parseEvalPhases (Loc.Located _ (SExpr.SList phases)) = mapM toPhase phases
  where
    toPhase (Loc.Located _ (SExpr.SUSym "COMPILE-TOPLEVEL")) = Right PhaseCompileTop
    toPhase (Loc.Located _ (SExpr.SUSym "LOAD-TOPLEVEL")) = Right PhaseLoadTop
    toPhase (Loc.Located _ (SExpr.SUSym "EXECUTE")) = Right PhaseExecute
    toPhase (Loc.Located _ (SExpr.SAtom "COMPILE-TOPLEVEL")) = Right PhaseCompileTop
    toPhase (Loc.Located _ (SExpr.SAtom "LOAD-TOPLEVEL")) = Right PhaseLoadTop
    toPhase (Loc.Located _ (SExpr.SAtom "EXECUTE")) = Right PhaseExecute
    toPhase _ = Left "invalid eval-when phase"
parseEvalPhases _ = Left "invalid eval-when phase list"

expandExpr :: MacroTable -> Int -> SExpr.SExpr -> ExpandM SExpr.SExpr
expandExpr macros depth sx
  | depth > maxDepth = throwExpandError "macro expansion depth limit exceeded"
  | otherwise = case Loc.locVal sx of
      SExpr.SList (Loc.Located _ (SExpr.SAtom name) : args)
        | Just clause <- M.lookup name macros -> do
            expanded <- applyMacro clause args name
            expandExpr macros (depth + 1) expanded
      SExpr.SList (kw@(Loc.Located _ (SExpr.SAtom "LET")) : rest) ->
        expandLet macros depth sx kw rest
      SExpr.SList (kw@(Loc.Located _ (SExpr.SAtom "LAM")) : rest) ->
        expandLam macros depth sx kw rest
      SExpr.SList (kw@(Loc.Located _ (SExpr.SAtom "CASE")) : rest) ->
        expandCase macros depth sx kw rest
      SExpr.SList (kw@(Loc.Located _ (SExpr.SAtom "INST")) : rest) ->
        expandInst macros depth sx kw rest
      SExpr.SList (Loc.Located _ (SExpr.SAtom tag) : _)
        | tag `elem` nonRecursiveForms ->
            pure sx
      SExpr.SList elems -> do
        elems' <- mapM (expandExpr macros depth) elems
        pure (Loc.Located (Loc.locSpan sx) (SExpr.SList elems'))
      _ -> pure sx

applyMacro :: MacroClause -> [SExpr.SExpr] -> T.Text -> ExpandM SExpr.SExpr
applyMacro clause args name =
  case matchClause clause args of
    Just bindings -> do
      mark <- State.get
      State.put (mark + 1)
      let mvalBinds = M.map MI.sexprToVal bindings
          env = M.union mvalBinds (mcEnv clause)
      result <- liftEither (firstLeft id (MI.runInterpMWithMark mark (MI.evalTyped env (mcTypedBody clause))))
      case MI.valToSExprHygienic result of
        Left err -> throwExpandError err
        Right sexpr -> pure sexpr
    Nothing ->
      throwExpandError ("macro " ++ T.unpack name
        ++ " does not accept " ++ show (length args) ++ " argument(s)")

matchClause :: MacroClause -> [SExpr.SExpr] -> Maybe Bindings
matchClause clause args = go (mcParams clause) args M.empty
  where
    go [] [] binds = Just binds
    go [] _ _ = Nothing
    go (ParamRest name : _) restArgs binds =
      Just (M.insert name (Loc.Located dummySpan (SExpr.SList restArgs)) binds)
    go (ParamSingle name : ps) (a : as) binds =
      go ps as (M.insert name a binds)
    go (ParamDestructure subPs : ps) (a : as) binds =
      case Loc.locVal a of
        SExpr.SList subArgs -> case go subPs subArgs binds of
          Just binds' -> go ps as binds'
          Nothing -> Nothing
        _ -> Nothing
    go _ _ _ = Nothing

compileMacroBody :: CompileState -> [MacroParam] -> SExpr.SExpr -> Either String TC.TRExpr
compileMacroBody st params template = do
  expr <- firstLeft SExpr.ceMsg (SExpr.toCompileExpr template)
  let paramCtx = M.fromList [(name, TC.Forall S.empty Ty.TySyntax) | name <- macroParamNames params]
      ctx = M.union paramCtx (csCtCtx st)
      resolveScope = M.keysSet ctx
  resolved <- firstLeft renderResolveErrs (Resolve.resolveWith resolveScope M.empty [expr])
  (typed, _) <- firstLeft renderTypeErrs (TC.typecheckWith (csCtEnvs st) ctx resolved)
  case reverse typed of
    trExpr : _ -> Right trExpr
    [] -> Left "invalid macro body"
  where
    renderResolveErrs errs = unlines (map Resolve.errMsg errs)
    renderTypeErrs errs = unlines (map TC.teMsg errs)

macroParamNames :: [MacroParam] -> [T.Text]
macroParamNames [] = []
macroParamNames (ParamSingle name : rest) = name : macroParamNames rest
macroParamNames (ParamRest name : _) = [name]
macroParamNames (ParamDestructure sub : rest) = macroParamNames sub ++ macroParamNames rest

extractMacroDefs :: [SExpr.SExpr] -> [SExpr.SExpr]
extractMacroDefs = filter isMacDef
  where
    isMacDef (Loc.Located _ (SExpr.SList (Loc.Located _ (SExpr.SAtom "MAC") : _))) = True
    isMacDef _ = False

moduleNameOrUser :: [SExpr.SExpr] -> T.Text
moduleNameOrUser sexprs = maybe "USER" id (SExpr.preScanModuleName sexprs)

dummySpan :: Loc.Span
dummySpan = Loc.Span (Loc.Pos "" 0 0) (Loc.Pos "" 0 0)

nonRecursiveForms :: [T.Text]
nonRecursiveForms =
  [ "TYPE"
  , "CLS"
  , "MODULE"
  , "IMPORT"
  , "FFI"
  , "FFI-STRUCT"
  , "FFI-VAR"
  , "FFI-ENUM"
  , "FFI-CALLBACK"
  , "EVAL-WHEN"
  ]

isImmediateRuntimeDecl :: SExpr.SExpr -> Bool
isImmediateRuntimeDecl (Loc.Located _ (SExpr.SList (Loc.Located _ (SExpr.SAtom tag) : _))) =
  tag `elem`
    [ "TYPE"
    , "CLS"
    , "INST"
    , "FFI"
    , "FFI-STRUCT"
    , "FFI-VAR"
    , "FFI-ENUM"
    , "FFI-CALLBACK"
    ]
isImmediateRuntimeDecl _ = False

isCompileVisibleRuntimeForm :: SExpr.SExpr -> Bool
isCompileVisibleRuntimeForm sx =
  isImmediateRuntimeDecl sx || isCompileVisibleRuntimeLet sx

compileVisibleRuntimeBindingNames :: SExpr.SExpr -> [T.Text]
compileVisibleRuntimeBindingNames (Loc.Located _ (SExpr.SList
  [ Loc.Located _ (SExpr.SAtom "LET")
  , Loc.Located _ (SExpr.SList binds)
  , body
  ]))
  | isCompileVisibleRuntimeLet (Loc.Located dummySpan (SExpr.SList
      [ Loc.Located dummySpan (SExpr.SAtom "LET")
      , Loc.Located dummySpan (SExpr.SList binds)
      , body
      ])) = foldr collect [] binds
  | otherwise = []
  where
    collect (Loc.Located _ (SExpr.SList [Loc.Located _ (SExpr.SAtom name), _])) acc =
      name : acc
    collect (Loc.Located _ (SExpr.SList [Loc.Located _ (SExpr.SAtom name), Loc.Located _ (SExpr.SType _), _])) acc =
      name : acc
    collect _ acc = acc
compileVisibleRuntimeBindingNames _ = []

isCompileVisibleRuntimeLet :: SExpr.SExpr -> Bool
isCompileVisibleRuntimeLet (Loc.Located _ (SExpr.SList
  [ Loc.Located _ (SExpr.SAtom "LET")
  , Loc.Located _ (SExpr.SList binds)
  , body
  ])) =
    case Loc.locVal body of
      SExpr.SAtom "UNIT" -> not (null (bindingNames binds))
      SExpr.SAtom name -> name `elem` bindingNames binds
      _ -> False
  where
    bindingNames = foldr collect []
    collect (Loc.Located _ (SExpr.SList [Loc.Located _ (SExpr.SAtom name), _])) acc =
      name : acc
    collect (Loc.Located _ (SExpr.SList [Loc.Located _ (SExpr.SAtom name), Loc.Located _ (SExpr.SType _), _])) acc =
      name : acc
    collect _ acc = acc
isCompileVisibleRuntimeLet _ = False

expandLet :: MacroTable -> Int -> SExpr.SExpr -> SExpr.SExpr -> [SExpr.SExpr] -> ExpandM SExpr.SExpr
expandLet macros depth sx kw [Loc.Located bindSp (SExpr.SList binds), body] = do
  binds' <- mapM (expandLetBinding macros depth) binds
  body' <- expandExpr macros depth body
  pure $ Loc.Located (Loc.locSpan sx) (SExpr.SList [kw, Loc.Located bindSp (SExpr.SList binds'), body'])
expandLet macros depth sx _ elems = expandListElems macros depth sx elems

expandLetBinding :: MacroTable -> Int -> SExpr.SExpr -> ExpandM SExpr.SExpr
expandLetBinding macros depth (Loc.Located sp (SExpr.SList [name, val])) = do
  val' <- expandExpr macros depth val
  pure $ Loc.Located sp (SExpr.SList [name, val'])
expandLetBinding macros depth (Loc.Located sp (SExpr.SList [name, ty@(Loc.Located _ (SExpr.SType _)), val])) = do
  val' <- expandExpr macros depth val
  pure $ Loc.Located sp (SExpr.SList [name, ty, val'])
expandLetBinding _ _ binding = pure binding

expandLam :: MacroTable -> Int -> SExpr.SExpr -> SExpr.SExpr -> [SExpr.SExpr] -> ExpandM SExpr.SExpr
expandLam macros depth sx kw [params@(Loc.Located _ (SExpr.SList _)), body] = do
  params' <- expandLamParams macros depth params
  body' <- expandExpr macros depth body
  pure $ Loc.Located (Loc.locSpan sx) (SExpr.SList [kw, params', body'])
expandLam macros depth sx kw [params@(Loc.Located _ (SExpr.SList _)), retTy@(Loc.Located _ (SExpr.SType _)), body] = do
  params' <- expandLamParams macros depth params
  body' <- expandExpr macros depth body
  pure $ Loc.Located (Loc.locSpan sx) (SExpr.SList [kw, params', retTy, body'])
expandLam macros depth sx _ elems = expandListElems macros depth sx elems

expandLamParams :: MacroTable -> Int -> SExpr.SExpr -> ExpandM SExpr.SExpr
expandLamParams macros depth (Loc.Located sp (SExpr.SList params)) = do
  params' <- go params
  pure $ Loc.Located sp (SExpr.SList params')
  where
    go [] = pure []
    go (marker@(Loc.Located _ (SExpr.SType (Loc.Located _ (SExpr.SAtom "OPT")))) : defs) = do
      defs' <- mapM (expandDefaultParam macros depth) defs
      pure (marker : defs')
    go (marker@(Loc.Located _ (SExpr.SAtom "&KEY")) : defs) = do
      defs' <- mapM (expandDefaultParam macros depth) defs
      pure (marker : defs')
    go (param : rest) = do
      rest' <- go rest
      pure (param : rest')
expandLamParams _ _ params = pure params

expandDefaultParam :: MacroTable -> Int -> SExpr.SExpr -> ExpandM SExpr.SExpr
expandDefaultParam macros depth (Loc.Located sp (SExpr.SList [name, val])) = do
  val' <- expandExpr macros depth val
  pure $ Loc.Located sp (SExpr.SList [name, val'])
expandDefaultParam macros depth (Loc.Located sp (SExpr.SList [name, ty@(Loc.Located _ (SExpr.SType _)), val])) = do
  val' <- expandExpr macros depth val
  pure $ Loc.Located sp (SExpr.SList [name, ty, val'])
expandDefaultParam _ _ param = pure param

expandCase :: MacroTable -> Int -> SExpr.SExpr -> SExpr.SExpr -> [SExpr.SExpr] -> ExpandM SExpr.SExpr
expandCase macros depth sx kw (scrutinee : arms) = do
  scrutinee' <- expandExpr macros depth scrutinee
  arms' <- mapM (expandCaseArm macros depth) arms
  pure $ Loc.Located (Loc.locSpan sx) (SExpr.SList (kw : scrutinee' : arms'))
expandCase macros depth sx _ elems = expandListElems macros depth sx elems

expandCaseArm :: MacroTable -> Int -> SExpr.SExpr -> ExpandM SExpr.SExpr
expandCaseArm macros depth (Loc.Located sp (SExpr.SList [pat, body])) = do
  body' <- expandExpr macros depth body
  pure $ Loc.Located sp (SExpr.SList [pat, body'])
expandCaseArm _ _ arm = pure arm

expandInst :: MacroTable -> Int -> SExpr.SExpr -> SExpr.SExpr -> [SExpr.SExpr] -> ExpandM SExpr.SExpr
expandInst macros depth sx kw (className : ty : methods) = do
  methods' <- mapM (expandInstMethod macros depth) methods
  pure $ Loc.Located (Loc.locSpan sx) (SExpr.SList (kw : className : ty : methods'))
expandInst macros depth sx _ elems = expandListElems macros depth sx elems

expandInstMethod :: MacroTable -> Int -> SExpr.SExpr -> ExpandM SExpr.SExpr
expandInstMethod macros depth (Loc.Located sp (SExpr.SList [name, body])) = do
  body' <- expandExpr macros depth body
  pure $ Loc.Located sp (SExpr.SList [name, body'])
expandInstMethod _ _ method = pure method

expandListElems :: MacroTable -> Int -> SExpr.SExpr -> [SExpr.SExpr] -> ExpandM SExpr.SExpr
expandListElems macros depth sx elems = do
  elems' <- mapM (expandExpr macros depth) elems
  pure $ Loc.Located (Loc.locSpan sx) (SExpr.SList elems')
