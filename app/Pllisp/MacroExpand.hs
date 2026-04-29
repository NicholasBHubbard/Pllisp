{-# LANGUAGE OverloadedStrings #-}

module Pllisp.MacroExpand
  ( CompileState(..)
  , ModuleResult(..)
  , defaultState
  , primitiveState
  , mergeCompileStates
  , expand
  , expandWith
  , expandModuleWith
  , extractMacroDefs
  ) where

import Control.Monad (foldM)
import qualified Control.Monad.State.Strict as State
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import System.IO.Unsafe (unsafePerformIO)

import qualified Pllisp.MacroInterp as MI
import qualified Pllisp.SExpr as SExpr
import qualified Pllisp.SrcLoc as Loc
import qualified Pllisp.Stdlib as Stdlib

data MacroClause = MacroClause
  { mcParams :: [MacroParam]
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

primitiveState :: CompileState
primitiveState =
  CompileState
    { csEnv = MI.defaultEnv
    , csMacros = M.empty
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
    Right result -> pure (mrState result)

emptyState :: CompileState
emptyState =
  CompileState
    { csEnv = M.empty
    , csMacros = M.empty
    , csEnvOrigins = M.empty
    , csMacroOrigins = M.empty
    }

throwExpandError :: String -> ExpandM a
throwExpandError = State.StateT . const . Left

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
      foldM mergeMacroEntry acc1 (M.toList (csMacros st))

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
    pure (st, [expanded])

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
    pure (st', if doEmit then [expanded] else [])

compileTopLevelForm :: T.Text -> CompileState -> SExpr.SExpr -> ExpandM CompileState
compileTopLevelForm modName st sx = do
  env' <- MI.loadTopLevelForm (csEnv st) sx
  let names = topLevelBindingNames sx
      origins' = foldl (\acc name -> M.insert name modName acc) (csEnvOrigins st) names
  pure st { csEnv = env', csEnvOrigins = origins' }

topLevelBindingNames :: SExpr.SExpr -> [T.Text]
topLevelBindingNames (Loc.Located _ (SExpr.SList
  [ Loc.Located _ (SExpr.SAtom "LET")
  , Loc.Located _ (SExpr.SList binds)
  , _
  ])) = mapMaybeBinding binds
topLevelBindingNames _ = []

mapMaybeBinding :: [SExpr.SExpr] -> [T.Text]
mapMaybeBinding [] = []
mapMaybeBinding (Loc.Located _ (SExpr.SList (Loc.Located _ (SExpr.SAtom name) : _)) : rest) =
  name : mapMaybeBinding rest
mapMaybeBinding (_ : rest) = mapMaybeBinding rest

defineMacro :: T.Text -> CompileState -> [SExpr.SExpr] -> ExpandM CompileState
defineMacro modName st macRest =
  case parseMacDef macRest of
    Left err -> throwExpandError err
    Right (name, params, template)
      | M.member name (csMacros st) ->
          throwExpandError ("duplicate macro definition: " ++ T.unpack name)
      | otherwise ->
          pure st
            { csMacros = M.insert name (MacroClause params template (csEnv st)) (csMacros st)
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
      result <- MI.eval env (mcTemplate clause)
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

extractMacroDefs :: [SExpr.SExpr] -> [SExpr.SExpr]
extractMacroDefs = filter isMacDef
  where
    isMacDef (Loc.Located _ (SExpr.SList (Loc.Located _ (SExpr.SAtom "MAC") : _))) = True
    isMacDef _ = False

liftEither :: Either String a -> ExpandM a
liftEither = either throwExpandError pure

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
