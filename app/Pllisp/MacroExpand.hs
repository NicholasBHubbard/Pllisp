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
  , mcTypedBody :: Maybe TC.TRExpr
  , mcTemplate :: SExpr.SExpr
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
  [ ("CAR", datumFun [Ty.TyDatum] Ty.TyDatum)
  , ("CDR", datumFun [Ty.TyDatum] Ty.TyDatum)
  , ("CONS", datumFun [Ty.TyDatum, Ty.TyDatum] Ty.TyDatum)
  , ("LENGTH", datumFun [Ty.TyDatum] Ty.TyInt)
  , ("NULL?", datumFun [Ty.TyDatum] Ty.TyBool)
  , ("SYMBOL?", datumFun [Ty.TyDatum] Ty.TyBool)
  , ("LIST?", datumFun [Ty.TyDatum] Ty.TyBool)
  , ("STRING?", datumFun [Ty.TyDatum] Ty.TyBool)
  , ("NUMBER?", datumFun [Ty.TyDatum] Ty.TyBool)
  , ("BOOL?", datumFun [Ty.TyDatum] Ty.TyBool)
  , ("TYPE?", datumFun [Ty.TyDatum] Ty.TyBool)
  , ("SYM-TO-STR", datumFun [Ty.TyDatum] Ty.TyStr)
  , ("STR-TO-SYM", datumFun [Ty.TyStr] Ty.TyDatum)
  , ("USYM-TO-STR", datumFun [Ty.TyDatum] Ty.TyStr)
  , ("STR-TO-USYM", datumFun [Ty.TyStr] Ty.TyDatum)
  , ("GENSYM", datumFun [] Ty.TyDatum)
  , ("EQ", TC.Forall (S.singleton 0) (Ty.TyFun [Ty.TyVar 0, Ty.TyVar 0] Ty.TyBool))
  , ("ERROR", TC.Forall (S.singleton 0) (Ty.TyFun [Ty.TyStr] (Ty.TyVar 0)))
  , ("__CT-LIFT", TC.Forall (S.singleton 0) (Ty.TyFun [Ty.TyVar 0] Ty.TyDatum))
  , ("__CT-ATOM", datumFun [Ty.TyStr] Ty.TyDatum)
  , ("__CT-INT", datumFun [Ty.TyInt] Ty.TyDatum)
  , ("__CT-FLT", datumFun [Ty.TyFlt] Ty.TyDatum)
  , ("__CT-STR", datumFun [Ty.TyStr] Ty.TyDatum)
  , ("__CT-BOOL", datumFun [Ty.TyBool] Ty.TyDatum)
  , ("__CT-USYM", datumFun [Ty.TyStr] Ty.TyDatum)
  , ("__CT-RX", datumFun [Ty.TyStr, Ty.TyStr] Ty.TyDatum)
  , ("__CT-TYPE", datumFun [Ty.TyDatum] Ty.TyDatum)
  , ("__CT-NIL", TC.Forall S.empty Ty.TyDatum)
  , ("__CT-CONS", datumFun [Ty.TyDatum, Ty.TyDatum] Ty.TyDatum)
  , ("__CT-APPEND", datumFun [Ty.TyDatum, Ty.TyDatum] Ty.TyDatum)
  ]
  where
    datumFun args ret = TC.Forall S.empty (Ty.TyFun args ret)

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
  env' <- firstLeft id (MI.runInterpM (MI.loadTypedTopLevelForms (csEnv st) typed))
  let exports = collectSurfaceExports fullEnvs typed
      ctVisibleExports = M.difference (collectDeclarationExports fullEnvs typed) (csCtCtx st)
      ctEnv = mergeVisibleEnv (csEnv st) env' ctVisibleExports
      localExports = M.difference exports (csRtCtx st)
      origins' = M.union (M.fromList [(name, modName) | name <- M.keys localExports]) (csEnvOrigins st)
  pure st
    { csEnv = ctEnv
    , csCtCtx = M.union ctVisibleExports (csCtCtx st)
    , csCtEnvs = TC.mergeTCEnvs (csCtEnvs st) fullEnvs
    , csRtCtx = M.union exports (csRtCtx st)
    , csRtEnvs = fullEnvs
    , csEnvOrigins = origins'
    }
  where
    renderResolveErrs errs = unlines (map Resolve.errMsg errs)
    renderTypeErrs errs = unlines (map TC.teMsg errs)

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
    st' <- if isImmediateRuntimeDecl expanded
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
    st'' <- if doEmit && isImmediateRuntimeDecl expanded
      then registerRuntimeDeclForm modName st' expanded
      else pure st'
    pure (st'', if doEmit then [expanded] else [])

compileTopLevelForm :: T.Text -> CompileState -> SExpr.SExpr -> ExpandM CompileState
compileTopLevelForm modName st sx = do
  expr <- liftEither (firstLeft SExpr.ceMsg (SExpr.toCompileExpr sx))
  let resolveScope = M.keysSet (csCtCtx st)
  resolved <- liftEither (firstLeft renderResolveErrs (Resolve.resolveWith resolveScope M.empty [expr]))
  (typed, fullEnvs) <- liftEither (firstLeft renderTypeErrs (TC.typecheckWith (csCtEnvs st) (csCtCtx st) resolved))
  env' <- liftEither (firstLeft id (MI.runInterpM (MI.loadTypedTopLevelForms (csEnv st) typed)))
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
  env' <- liftEither (firstLeft id (MI.runInterpM (MI.loadTypedTopLevelForms (csEnv st) typed)))
  let exports = collectSurfaceExports fullEnvs typed
      ctVisibleExports = M.difference (collectDeclarationExports fullEnvs typed) (csCtCtx st)
      ctEnv = mergeVisibleEnv (csEnv st) env' ctVisibleExports
      localExports = M.difference exports (csRtCtx st)
      names = M.keys localExports
      origins' = foldl (\acc name -> M.insert name modName acc) (csEnvOrigins st) names
  pure st
    { csEnv = ctEnv
    , csCtCtx = M.union ctVisibleExports (csCtCtx st)
    , csCtEnvs = TC.mergeTCEnvs (csCtEnvs st) fullEnvs
    , csRtCtx = M.union exports (csRtCtx st)
    , csRtEnvs = fullEnvs
    , csEnvOrigins = origins'
    }
  where
    renderResolveErrs errs = unlines (map Resolve.errMsg errs)
    renderTypeErrs errs = unlines (map TC.teMsg errs)

defineMacro :: T.Text -> CompileState -> [SExpr.SExpr] -> ExpandM CompileState
defineMacro modName st macRest =
  case parseMacDef macRest of
    Left err -> throwExpandError err
    Right (name, params, template)
      | M.member name (csMacros st) ->
          throwExpandError ("duplicate macro definition: " ++ T.unpack name)
      | otherwise -> do
          let typedBody = either (const Nothing) Just (compileMacroBody st params template)
          pure st
            { csMacros = M.insert name (MacroClause params typedBody template (csEnv st)) (csMacros st)
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
      let mvalBinds = M.map MI.sexprToVal bindings
          env = M.union mvalBinds (mcEnv clause)
      result <- case mcTypedBody clause of
        Just body ->
          liftEither (firstLeft id (MI.runInterpM (MI.evalTyped env body)))
        Nothing ->
          MI.eval env (mcTemplate clause)
      case MI.valToSExpr result of
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
  let paramCtx = M.fromList [(name, TC.Forall S.empty Ty.TyDatum) | name <- macroParamNames params]
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
