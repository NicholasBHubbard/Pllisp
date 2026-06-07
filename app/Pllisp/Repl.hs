{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Pllisp.Repl
  ( ReplConfig(..)
  , defaultConfig
  , ReplSession
  , ReplError(..)
  , ReplExecResult(..)
  , ReplTypeInfo(..)
  , ReplMacroInfo(..)
  , newSession
  , closeSession
  , resetSession
  , reloadSession
  , submitForms
  , loadFile
  , typeOf
  , macroExpand
  ) where

import Control.Exception (SomeException, bracket, finally, try)
import Data.IORef
import Data.Maybe (listToMaybe)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (FunPtr, Ptr, nullPtr)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.Directory
  ( createDirectory
  , getCurrentDirectory
  , getTemporaryDirectory
  , removeFile
  , removePathForcibly
  )
import System.FilePath (isAbsolute, takeDirectory, takeFileName, (</>))
import System.IO
  ( BufferMode(..)
  , hClose
  , hFlush
  , hSetBuffering
  , openTempFile
  , stdout
  )
import System.Posix.DynamicLinker
  ( DL
  , RTLDFlags(..)
  , dlclose
  , dlopen
  , dlsym
  )
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

import qualified Text.Megaparsec as MP

import qualified Pllisp.CST as CST
import qualified Pllisp.Codegen as Codegen
import qualified Pllisp.ClosureConvert as CC
import qualified Pllisp.Error as Error
import qualified Pllisp.LambdaLift as LL
import qualified Pllisp.MacroExpand as MacroExpand
import qualified Pllisp.Module as Mod
import qualified Pllisp.Parser as Parser
import qualified Pllisp.Pipeline as Pipeline
import qualified Pllisp.Resolve as Resolve
import qualified Pllisp.SExpr as SExpr
import qualified Pllisp.SrcLoc as Loc
import qualified Pllisp.Stdlib as Stdlib
import qualified Pllisp.Type as Ty
import qualified Pllisp.TypeCheck as TC

foreign import ccall "dynamic"
  mkReplEntry :: FunPtr (IO ()) -> IO ()

foreign import ccall "fflush"
  c_fflush :: Ptr a -> IO CInt

data ReplConfig = ReplConfig
  { rcWorkDir :: Maybe FilePath
  , rcKeepArtifacts :: Bool
  }

defaultConfig :: ReplConfig
defaultConfig =
  ReplConfig
    { rcWorkDir = Nothing
    , rcKeepArtifacts = False
    }

data ReplError = ReplError
  { rePhase :: T.Text
  , reMessage :: T.Text
  } deriving (Eq, Show)

newtype ReplSession = ReplSession (IORef SessionState)

data ReplExecResult = ReplExecResult
  { reStdout :: T.Text
  } deriving (Eq, Show)

data ReplTypeInfo = ReplTypeInfo
  { rtiType :: Ty.Type
  , rtiRendered :: T.Text
  } deriving (Eq, Show)

data ReplMacroInfo = ReplMacroInfo
  { rmiExpanded :: [SExpr.SExpr]
  , rmiRendered :: T.Text
  } deriving (Eq, Show)

data LoadedModule = LoadedModule
  { lmExports :: M.Map CST.Symbol TC.Scheme
  , lmTyped :: TC.TResolvedCST
  , lmEnvs :: TC.TCEnvs
  , lmCompileState :: MacroExpand.CompileState
  , lmGlobals :: [(CST.Symbol, Ty.Type)]
  , lmMeta :: [LL.LLExpr]
  }

newtype BundleArtifact = BundleArtifact FilePath

data PreparedImports = PreparedImports
  { piImports :: [CST.Import]
  , piLoadedModules :: M.Map CST.Symbol LoadedModule
  , piNewModules :: [Pipeline.CompiledModule]
  , piBundle :: Maybe BundleArtifact
  , piNextId :: Int
  , piBaseCtState :: MacroExpand.CompileState
  }

data SessionState = SessionState
  { rsConfig :: ReplConfig
  , rsSymbolPrefix :: T.Text
  , rsTempRoot :: FilePath
  , rsStdlibDir :: FilePath
  , rsSearchDir :: FilePath
  , rsNextId :: Int
  , rsLastLoadedFile :: Maybe FilePath
  , rsHandles :: [DL]
  , rsImports :: [CST.Import]
  , rsLoadedModules :: M.Map CST.Symbol LoadedModule
  , rsLocalScope :: S.Set CST.Symbol
  , rsLocalContext :: TC.Context
  , rsLocalEnvs :: TC.TCEnvs
  , rsLocalGlobals :: [(CST.Symbol, Ty.Type)]
  , rsCtState :: MacroExpand.CompileState
  , rsPrevExprs :: [CST.Expr]
  }

newSession :: ReplConfig -> IO ReplSession
newSession cfg = do
  state <- freshSessionState cfg Nothing Nothing
  ReplSession <$> newIORef state

closeSession :: ReplSession -> IO ()
closeSession (ReplSession ref) = readIORef ref >>= teardownState

resetSession :: ReplSession -> IO ()
resetSession (ReplSession ref) = do
  old <- readIORef ref
  fresh <- freshSessionState (rsConfig old) (Just (rsSearchDir old)) (rsLastLoadedFile old)
  writeIORef ref fresh
  teardownState old

reloadSession :: ReplSession -> IO (Either ReplError ReplExecResult)
reloadSession sess@(ReplSession ref) = do
  st <- readIORef ref
  case rsLastLoadedFile st of
    Nothing -> pure (Left (ReplError "reload" "no file loaded"))
    Just fp -> loadFile sess fp

loadFile :: ReplSession -> FilePath -> IO (Either ReplError ReplExecResult)
loadFile sess@(ReplSession ref) fp = do
  st <- readIORef ref
  let resolved = resolvePath (rsSearchDir st) fp
      searchDir = takeDirectory resolved
  srcResult <- try (T.IO.readFile resolved) :: IO (Either SomeException T.Text)
  case srcResult of
    Left ex -> pure (Left (ReplError "load" (T.pack (show ex))))
    Right src -> do
      fresh <- freshSessionState (rsConfig st) (Just searchDir) (Just resolved)
      writeIORef ref fresh
      teardownState st
      submitFormsAt sess resolved searchDir src

submitForms :: ReplSession -> T.Text -> IO (Either ReplError ReplExecResult)
submitForms sess@(ReplSession ref) src = do
  st <- readIORef ref
  submitFormsAt sess "<repl>" (rsSearchDir st) src

typeOf :: ReplSession -> T.Text -> IO (Either ReplError ReplTypeInfo)
typeOf (ReplSession ref) src = do
  st <- readIORef ref
  case Parser.parseSExprs "<type>" src of
    Left err -> pure (Left (ReplError "parse" (T.pack (MP.errorBundlePretty err))))
    Right sexprs
      | hasModuleDecl sexprs ->
          pure (Left (ReplError "type" "module declarations are not allowed in the REPL"))
      | not (null (SExpr.preScanImports sexprs)) ->
          pure (Left (ReplError "type" "imports are not allowed in :type; import modules first"))
      | otherwise ->
          case MacroExpand.expandModuleWith "REPL" (rsCtState st) sexprs of
            Left err -> pure (Left (ReplError "macro" (T.pack err)))
            Right result ->
              case SExpr.toProgram (MacroExpand.mrExpanded result) of
                Left err -> pure (Left (ReplError "syntax" (T.pack (SExpr.ceMsg err))))
                Right prog ->
                  case typecheckExpr st src (CST.progExprs prog) of
                    Left err -> pure (Left err)
                    Right typedExpr ->
                      pure
                        (Right
                          ReplTypeInfo
                            { rtiType = Ty.ty (Loc.locVal typedExpr)
                            , rtiRendered = Ty.renderType (Ty.ty (Loc.locVal typedExpr))
                            })

macroExpand :: ReplSession -> T.Text -> IO (Either ReplError ReplMacroInfo)
macroExpand (ReplSession ref) src = do
  st <- readIORef ref
  case Parser.parseSExprs "<macroexpand>" src of
    Left err -> pure (Left (ReplError "parse" (T.pack (MP.errorBundlePretty err))))
    Right sexprs
      | hasModuleDecl sexprs ->
          pure (Left (ReplError "macroexpand" "module declarations are not allowed in the REPL"))
      | not (null (SExpr.preScanImports sexprs)) ->
          pure (Left (ReplError "macroexpand" "imports are not allowed in :macroexpand; import modules first"))
      | otherwise ->
          case MacroExpand.expandModuleWith "REPL" (rsCtState st) sexprs of
            Left err -> pure (Left (ReplError "macro" (T.pack err)))
            Right result ->
              let expanded = MacroExpand.mrExpanded result
              in pure
                   (Right
                     ReplMacroInfo
                       { rmiExpanded = expanded
                       , rmiRendered = T.intercalate "\n" (map renderSExpr expanded)
                       })

submitFormsAt :: ReplSession -> FilePath -> FilePath -> T.Text -> IO (Either ReplError ReplExecResult)
submitFormsAt (ReplSession ref) sourceName searchDir src = do
  st <- readIORef ref
  case Parser.parseSExprs sourceName src of
    Left err -> pure (Left (ReplError "parse" (T.pack (MP.errorBundlePretty err))))
    Right sexprs
      | hasModuleDecl sexprs ->
          pure (Left (ReplError "repl" "module declarations are not allowed in the REPL"))
      | otherwise -> do
          preparedResult <- prepareImports st searchDir (SExpr.preScanImports sexprs)
          case preparedResult of
            Left err -> pure (Left err)
            Right prepared ->
              case MacroExpand.expandModuleWith "REPL" (piBaseCtState prepared) sexprs of
                Left err -> pure (Left (ReplError "macro" (T.pack err)))
                Right expandedResult ->
                  case SExpr.toProgram (MacroExpand.mrExpanded expandedResult) of
                    Left err -> pure (Left (ReplError "syntax" (T.pack (SExpr.ceMsg err))))
                    Right prog -> do
                      let currentExprs = CST.progExprs prog
                          newDefs = filter isDefExpr currentExprs
                          nextCtState = MacroExpand.mrState expandedResult
                      roundBuild <-
                        if null currentExprs
                          then pure (Right Nothing)
                          else buildLocalRound st prepared src currentExprs
                      case roundBuild of
                        Left err -> pure (Left err)
                        Right maybeRound -> do
                          execResult <- executePrepared prepared maybeRound
                          case execResult of
                            Left err -> pure (Left err)
                            Right (newHandles, out, maybeLocalState) -> do
                              let committed =
                                    commitSession
                                      st
                                      prepared
                                      searchDir
                                      nextCtState
                                      newDefs
                                      newHandles
                                      maybeLocalState
                              writeIORef ref committed
                              pure (Right (ReplExecResult out))

data LocalRound = LocalRound
  { lrArtifact :: BundleArtifact
  , lrNextId :: Int
  , lrGlobals :: [(CST.Symbol, Ty.Type)]
  , lrEnvs :: TC.TCEnvs
  }

buildLocalRound
  :: SessionState
  -> PreparedImports
  -> T.Text
  -> [CST.Expr]
  -> IO (Either ReplError (Maybe LocalRound))
buildLocalRound st prepared src currentExprs = do
  let loaded = piLoadedModules prepared
      visibleImports = defaultPreludeImport loaded : piImports prepared
      loadedExports = M.map lmExports loaded
      (importScope, importCtx, normMap) = Mod.buildImportScope loadedExports visibleImports
      visibleScope = S.union importScope (rsLocalScope st)
      visibleCtx = M.union (rsLocalContext st) importCtx
      visibleEnvs = TC.mergeTCEnvs (aggregateModuleEnvs loaded) (rsLocalEnvs st)
      sourceExprs = rsPrevExprs st ++ currentExprs
      priorGlobals = aggregateModuleGlobals loaded ++ rsLocalGlobals st
      importedMeta = aggregateModuleMeta loaded
      existingNames = S.fromList (map fst priorGlobals)
  case Mod.desugarTopLevel sourceExprs of
    Left err -> pure (Left (ReplError "desugar" (T.pack err)))
    Right exprs ->
      case Resolve.resolveWith visibleScope normMap exprs of
        Left errs ->
          pure (Left (renderResolveErrors src errs))
        Right resolved ->
          case TC.typecheckWith visibleEnvs visibleCtx resolved of
            Left errs ->
              pure (Left (renderTypeErrors src errs))
            Right (typed, roundEnvs) -> do
              let merged = Mod.mergeImportedCode (map lmTyped (M.elems loaded)) typed
              case Pipeline.validateRuntimeSyntaxTypes merged of
                Just (_, msg) -> pure (Left (ReplError "type" (T.pack msg)))
                Nothing -> do
                  compileResult <- compileBundleArtifact
                    (rsSymbolPrefix st)
                    (rsTempRoot st)
                    (piNextId prepared)
                    priorGlobals
                    importedMeta
                    typed
                  case compileResult of
                    Left err -> pure (Left err)
                    Right artifact -> do
                      let llProg = LL.lambdaLift (CC.closureConvert typed)
                          allNewGlobals = dedupGlobals (Codegen.collectReplGlobals (LL.llExprs llProg))
                          newGlobals = filter (\(n, _) -> not (S.member n existingNames)) allNewGlobals
                      pure
                        (Right
                          (Just
                            LocalRound
                              { lrArtifact = artifact
                              , lrNextId = piNextId prepared + 1
                              , lrGlobals = newGlobals
                              , lrEnvs = roundEnvs
                              }))

executePrepared
  :: PreparedImports
  -> Maybe LocalRound
  -> IO (Either ReplError ([DL], T.Text, Maybe LocalRound))
executePrepared prepared maybeLocal = do
  importExec <- case piBundle prepared of
    Nothing -> pure (Right ([], ""))
    Just artifact -> do
      result <- executeArtifact artifact
      pure (fmap (\(hdl, out) -> ([hdl], out)) result)
  case importExec of
    Left err -> pure (Left err)
    Right (importHandles, importOut) ->
      case maybeLocal of
        Nothing -> pure (Right (importHandles, importOut, Nothing))
        Just localRound -> do
          localExec <- executeArtifact (lrArtifact localRound)
          case localExec of
            Left err -> pure (Left err)
            Right (localHandle, localOut) ->
              pure (Right (importHandles ++ [localHandle], importOut <> localOut, Just localRound))

commitSession
  :: SessionState
  -> PreparedImports
  -> FilePath
  -> MacroExpand.CompileState
  -> [CST.Expr]
  -> [DL]
  -> Maybe LocalRound
  -> SessionState
commitSession st prepared searchDir nextCtState newDefs newHandles maybeLocal =
  let base =
        st
          { rsSearchDir = searchDir
          , rsImports = piImports prepared
          , rsLoadedModules = piLoadedModules prepared
          , rsCtState = nextCtState
          , rsPrevExprs = rsPrevExprs st ++ newDefs
          , rsHandles = rsHandles st ++ newHandles
          , rsNextId = maybe (piNextId prepared) lrNextId maybeLocal
          }
  in case maybeLocal of
       Nothing -> base
       Just local ->
         let newNames = S.fromList (map fst (lrGlobals local))
             newSchemes = M.fromList [(n, TC.Forall S.empty [] t) | (n, t) <- lrGlobals local]
         in base
              { rsLocalScope = S.union (rsLocalScope st) newNames
              , rsLocalContext = M.union newSchemes (rsLocalContext st)
              , rsLocalGlobals = rsLocalGlobals st ++ lrGlobals local
              , rsLocalEnvs = TC.mergeTCEnvs (rsLocalEnvs st) (lrEnvs local)
              }

prepareImports :: SessionState -> FilePath -> [CST.Import] -> IO (Either ReplError PreparedImports)
prepareImports st searchDir currentImports = do
  let pendingImports = dedupImports (rsImports st ++ currentImports)
      currentLoaded = rsLoadedModules st
  if null pendingImports
    then pure
      (Right
        PreparedImports
          { piImports = pendingImports
          , piLoadedModules = currentLoaded
          , piNewModules = []
          , piBundle = Nothing
          , piNextId = rsNextId st
          , piBaseCtState = rsCtState st
          })
    else do
      scanResult <- Pipeline.scanAllModules searchDir (rsStdlibDir st) pendingImports
      case scanResult of
        Left err -> pure (Left (ReplError "import" (T.pack err)))
        Right moduleInfos -> do
          let existingNames = M.keysSet currentLoaded
              newModuleInfos = M.filterWithKey (\name _ -> not (S.member name existingNames)) moduleInfos
          if M.null newModuleInfos
            then pure
              (Right
                PreparedImports
                  { piImports = pendingImports
                  , piLoadedModules = currentLoaded
                  , piNewModules = []
                  , piBundle = Nothing
                  , piNextId = rsNextId st
                  , piBaseCtState = rsCtState st
                  })
            else do
              let depMap = buildIncrementalDepMap existingNames newModuleInfos
              case Mod.dependencyOrder depMap of
                Left err -> pure (Left (ReplError "import" (T.pack err)))
                Right order -> do
                  let newOrder = filter (`M.member` newModuleInfos) order
                      seedStates = M.map lmCompileState currentLoaded
                  case Pipeline.expandModulesFrom seedStates newModuleInfos newOrder of
                    Left err -> pure (Left (ReplError "macro" (T.pack err)))
                    Right (expandedMap, compileStates) -> do
                      compiledResult <-
                        Pipeline.compileModulesDetailedFrom
                          (compileSeedFromLoaded currentLoaded)
                          expandedMap
                          compileStates
                          newModuleInfos
                          newOrder
                      case compiledResult of
                        Left err -> pure (Left (ReplError "import" (T.pack err)))
                        Right (compiledModules, _) -> do
                          let loadedNew = foldl' (\acc m -> M.insert (Pipeline.cmName m) (loadedFromCompiled m) acc) currentLoaded compiledModules
                          bundleResult <-
                            compileImportBundle
                              st
                              compiledModules
                          case bundleResult of
                            Left err -> pure (Left err)
                            Right bundle ->
                              case MacroExpand.mergeCompileStates (rsCtState st : map Pipeline.cmCompileState compiledModules) of
                                Left err -> pure (Left (ReplError "macro" (T.pack err)))
                                Right baseCtState ->
                                  pure
                                    (Right
                                      PreparedImports
                                        { piImports = pendingImports
                                        , piLoadedModules = loadedNew
                                        , piNewModules = compiledModules
                                        , piBundle = bundle
                                        , piNextId = rsNextId st + maybe 0 (const 1) bundle
                                        , piBaseCtState = baseCtState
                                        })

compileImportBundle :: SessionState -> [Pipeline.CompiledModule] -> IO (Either ReplError (Maybe BundleArtifact))
compileImportBundle _ [] = pure (Right Nothing)
compileImportBundle st modules =
  fmap Just <$>
    compileBundleArtifact
      (rsSymbolPrefix st)
      (rsTempRoot st)
      (rsNextId st)
      (aggregateModuleGlobals (rsLoadedModules st))
      (aggregateModuleMeta (rsLoadedModules st))
      (concatMap Pipeline.cmTyped modules)

compileBundleArtifact
  :: T.Text
  -> FilePath
  -> Int
  -> [(CST.Symbol, Ty.Type)]
  -> [LL.LLExpr]
  -> TC.TResolvedCST
  -> IO (Either ReplError BundleArtifact)
compileBundleArtifact symbolPrefix root bundleId priorGlobals importedMeta typed = do
  let llProg = LL.lambdaLift (CC.closureConvert typed)
      ir = Codegen.codegenRepl symbolPrefix bundleId priorGlobals importedMeta llProg
      stem = "pllisp_repl_" ++ show bundleId
      llFile = root </> stem ++ ".ll"
      bridgeFile = root </> stem ++ "_ffi_bridge.c"
      soFile = root </> stem ++ ".so"
  T.IO.writeFile llFile ir
  T.IO.writeFile bridgeFile Ty.ffiBridgeC
  (ec, _, err) <- readProcessWithExitCode "clang"
    [llFile, bridgeFile, "-shared", "-fPIC", "-o", soFile, "-lm", "-lpcre2-8", "-lgc", "-lffi"] ""
  ignoreIO (removeFile bridgeFile)
  case ec of
    ExitFailure _ -> pure (Left (ReplError "clang" (T.pack err)))
    ExitSuccess -> pure (Right (BundleArtifact soFile))

executeArtifact :: BundleArtifact -> IO (Either ReplError (DL, T.Text))
executeArtifact (BundleArtifact soFile) = do
  result <- try $ captureStdout (takeDirectory soFile) $ do
    handle <- dlopen soFile [RTLD_NOW, RTLD_GLOBAL]
    entry <- dlsym handle "pll_repl_entry"
    mkReplEntry entry
    _ <- c_fflush nullPtr
    pure handle
  case result of
    Left ex -> pure (Left (ReplError "runtime" (T.pack (show (ex :: SomeException)))))
    Right (out, handle) -> pure (Right (handle, out))

freshSessionState :: ReplConfig -> Maybe FilePath -> Maybe FilePath -> IO SessionState
freshSessionState cfg mSearchDir lastLoaded = do
  cwd <- maybe getCurrentDirectory pure (rcWorkDir cfg)
  let searchDir = maybe cwd id mSearchDir
  stdlibDir <- Stdlib.getStdlibDirNear [searchDir, cwd]
  root <- createTempDir "pllisp-repl"
  let symbolPrefix = T.pack (takeFileName root)
  prelude <- buildPreludeState cfg symbolPrefix root stdlibDir searchDir lastLoaded
  pure prelude

buildPreludeState :: ReplConfig -> T.Text -> FilePath -> FilePath -> FilePath -> Maybe FilePath -> IO SessionState
buildPreludeState cfg symbolPrefix root stdlibDir searchDir lastLoaded = do
  let preludePath = stdlibDir </> "PRELUDE.pll"
  src <- T.IO.readFile preludePath
  sexprs <- case Parser.parseSExprs preludePath src of
    Left err -> fail (MP.errorBundlePretty err)
    Right parsed -> pure parsed
  let infos = M.singleton "PRELUDE" (Pipeline.ModuleInfo preludePath sexprs [])
  (expandedMap, compileStates) <- case Pipeline.expandModulesFrom M.empty infos ["PRELUDE"] of
    Left err -> fail err
    Right ok -> pure ok
  compiledResult <- Pipeline.compileModulesDetailedFrom Pipeline.emptyCompileSeed expandedMap compileStates infos ["PRELUDE"]
  (compiledMods, _) <- case compiledResult of
    Left err -> fail err
    Right ok -> pure ok
  preludeModule <- case compiledMods of
    [modu] -> pure (loadedFromCompiled modu)
    _ -> fail "expected PRELUDE module"
  artifactResult <- compileBundleArtifact symbolPrefix root 0 [] [] (lmTyped preludeModule)
  artifact <- case artifactResult of
    Left err -> fail (T.unpack (reMessage err))
    Right ok -> pure ok
  execResult <- executeArtifact artifact
  handle <- case execResult of
    Left err -> fail (T.unpack (reMessage err))
    Right (hdl, _) -> pure hdl
  pure
    SessionState
      { rsConfig = cfg
      , rsSymbolPrefix = symbolPrefix
      , rsTempRoot = root
      , rsStdlibDir = stdlibDir
      , rsSearchDir = searchDir
      , rsNextId = 1
      , rsLastLoadedFile = lastLoaded
      , rsHandles = [handle]
      , rsImports = []
      , rsLoadedModules = M.singleton "PRELUDE" preludeModule
      , rsLocalScope = S.empty
      , rsLocalContext = M.empty
      , rsLocalEnvs = TC.emptyTCEnvs
      , rsLocalGlobals = []
      , rsCtState = lmCompileState preludeModule
      , rsPrevExprs = []
      }

loadedFromCompiled :: Pipeline.CompiledModule -> LoadedModule
loadedFromCompiled modu =
  let llProg = LL.lambdaLift (CC.closureConvert (Pipeline.cmTyped modu))
      rawGlobals = dedupGlobals (Codegen.collectReplGlobals (LL.llExprs llProg))
      exports = M.keysSet (Pipeline.cmExports modu)
  in LoadedModule
       { lmExports = Pipeline.cmExports modu
       , lmTyped = Pipeline.cmTyped modu
       , lmEnvs = Pipeline.cmEnvs modu
       , lmCompileState = Pipeline.cmCompileState modu
       , lmGlobals = filterModuleGlobals (Pipeline.cmName modu) exports rawGlobals
       , lmMeta = LL.llExprs llProg
       }

compileSeedFromLoaded :: M.Map CST.Symbol LoadedModule -> Pipeline.CompileSeed
compileSeedFromLoaded loaded =
  Pipeline.CompileSeed
    { Pipeline.seedExports = M.map lmExports loaded
    , Pipeline.seedTypedModules = map lmTyped (M.elems loaded)
    , Pipeline.seedEnvs = aggregateModuleEnvs loaded
    , Pipeline.seedCompileStates = M.map lmCompileState loaded
    }

aggregateModuleEnvs :: M.Map CST.Symbol LoadedModule -> TC.TCEnvs
aggregateModuleEnvs = foldl' TC.mergeTCEnvs TC.emptyTCEnvs . map lmEnvs . M.elems

aggregateModuleGlobals :: M.Map CST.Symbol LoadedModule -> [(CST.Symbol, Ty.Type)]
aggregateModuleGlobals = concatMap lmGlobals . M.elems

aggregateModuleMeta :: M.Map CST.Symbol LoadedModule -> [LL.LLExpr]
aggregateModuleMeta = concatMap lmMeta . M.elems

defaultPreludeImport :: M.Map CST.Symbol LoadedModule -> CST.Import
defaultPreludeImport loaded =
  case M.lookup "PRELUDE" loaded of
    Nothing -> CST.Import "PRELUDE" "PRELUDE" []
    Just prelude -> CST.Import "PRELUDE" "PRELUDE" (M.keys (lmExports prelude))

buildIncrementalDepMap :: S.Set CST.Symbol -> M.Map CST.Symbol Pipeline.ModuleInfo -> M.Map CST.Symbol [CST.Symbol]
buildIncrementalDepMap existing moduleInfos =
  let rawDepMap = M.map (map CST.impModule . Pipeline.miImports) moduleInfos
      newDeps =
        M.mapWithKey
          (\k ds ->
            if k == "PRELUDE" || "PRELUDE" `elem` ds
              then ds
              else "PRELUDE" : ds)
          rawDepMap
      existingDeps = M.fromSet (const []) existing
  in M.union newDeps existingDeps

typecheckExpr :: SessionState -> T.Text -> [CST.Expr] -> Either ReplError TC.TRExpr
typecheckExpr st src exprs =
  let loaded = rsLoadedModules st
      visibleImports = defaultPreludeImport loaded : rsImports st
      loadedExports = M.map lmExports loaded
      (importScope, importCtx, normMap) = Mod.buildImportScope loadedExports visibleImports
      visibleScope = S.union importScope (rsLocalScope st)
      visibleCtx = M.union (rsLocalContext st) importCtx
      visibleEnvs = TC.mergeTCEnvs (aggregateModuleEnvs loaded) (rsLocalEnvs st)
  in case listToMaybe exprs of
       Nothing ->
         Left (ReplError "type" "no expression to type")
       Just expr ->
         case Resolve.resolveWith visibleScope normMap [expr] of
           Left errs ->
             Left (renderResolveErrors src errs)
           Right [resolved] ->
             case TC.typecheckWith visibleEnvs visibleCtx [resolved] of
               Left errs ->
                 Left (renderTypeErrors src errs)
               Right (typedExprs, _) ->
                 case reverse typedExprs of
                   typedExpr:_ -> Right typedExpr
                   [] -> Left (ReplError "type" "no expression to type")
           Right _ ->
             Left (ReplError "type" "expected a single expression")

renderResolveErrors :: T.Text -> [Resolve.ResolveError] -> ReplError
renderResolveErrors src errs =
  ReplError
    "resolve"
    (T.pack
      (concatMap
        (\e -> Error.renderError src "resolve" (Resolve.errSpan e) (Resolve.errMsg e))
        errs))

renderTypeErrors :: T.Text -> [TC.TypeError] -> ReplError
renderTypeErrors src errs =
  ReplError
    "type"
    (T.pack
      (concatMap
        (\e -> Error.renderError src "type" (TC.teSpan e) (TC.teMsg e))
        errs))

captureStdout :: FilePath -> IO a -> IO (T.Text, a)
captureStdout dir action =
  bracket (openTempFile dir "pllisp-repl.out") cleanup $ \(fp, h) -> do
    old <- hDuplicate stdout
    hSetBuffering stdout LineBuffering
    result <-
      (`finally` restoreStdout old h) $ do
        hDuplicateTo h stdout
        value <- action
        hFlush stdout
        _ <- c_fflush nullPtr
        pure value
    out <- T.IO.readFile fp
    pure (out, result)
  where
    restoreStdout old h = do
      hFlush stdout
      _ <- c_fflush nullPtr
      hDuplicateTo old stdout
      ignoreIO (hClose old)
      ignoreIO (hClose h)

    cleanup (fp, h) = do
      ignoreIO (hClose h)
      ignoreIO (removeFile fp)

createTempDir :: String -> IO FilePath
createTempDir prefix = do
  tmp <- getTemporaryDirectory
  (fp, h) <- openTempFile tmp prefix
  hClose h
  removeFile fp
  createDirectory fp
  pure fp

teardownState :: SessionState -> IO ()
teardownState st = do
  mapM_ (ignoreIO . dlclose) (rsHandles st)
  if rcKeepArtifacts (rsConfig st)
    then pure ()
    else ignoreIO (removePathForcibly (rsTempRoot st))

hasModuleDecl :: [SExpr.SExpr] -> Bool
hasModuleDecl = maybe False (const True) . SExpr.preScanModuleName

isDefExpr :: CST.Expr -> Bool
isDefExpr (Loc.Located _ e) = case e of
  CST.ExprType {} -> True
  CST.ExprFFI {} -> True
  CST.ExprFFIStruct {} -> True
  CST.ExprFFIVar {} -> True
  CST.ExprFFIEnum {} -> True
  CST.ExprFFICallback {} -> True
  _ -> False

dedupImports :: [CST.Import] -> [CST.Import]
dedupImports = foldl' step []
  where
    step acc imp
      | imp `elem` acc = acc
      | otherwise = acc ++ [imp]

dedupGlobals :: [(CST.Symbol, Ty.Type)] -> [(CST.Symbol, Ty.Type)]
dedupGlobals = foldl' step []
  where
    step acc item@(name, _)
      | any ((== name) . fst) acc = acc
      | otherwise = acc ++ [item]

filterModuleGlobals :: CST.Symbol -> S.Set CST.Symbol -> [(CST.Symbol, Ty.Type)] -> [(CST.Symbol, Ty.Type)]
filterModuleGlobals modName _ globals
  | modName == "PRELUDE" = globals
  | otherwise = filter isOwned globals
  where
    modPrefix = T.toLower modName <> "."
    instPrefix = "__inst_" <> T.toLower modName <> "."

    isOwned (name, _) =
      let lower = T.toLower name
      in modPrefix `T.isPrefixOf` lower
           || instPrefix `T.isPrefixOf` lower

resolvePath :: FilePath -> FilePath -> FilePath
resolvePath base fp
  | isAbsolute fp = fp
  | otherwise = base </> fp

renderSExpr :: SExpr.SExpr -> T.Text
renderSExpr (Loc.Located _ sexprF) = case sexprF of
  SExpr.SAtom t -> t
  SExpr.SInt n -> T.pack (show n)
  SExpr.SFlt f -> T.pack (show f)
  SExpr.SStr t -> "\"" <> escapeStr t <> "\""
  SExpr.SRx pat flags -> "#/" <> escapeStr pat <> "/" <> flags
  SExpr.SList xs -> "(" <> T.intercalate " " (map renderSExpr xs) <> ")"
  SExpr.SType inner -> "%" <> renderSExpr inner
  SExpr.SQuasi inner -> "`" <> renderSExpr inner
  SExpr.SUnquote inner -> "," <> renderSExpr inner
  SExpr.SSplice inner -> ",@" <> renderSExpr inner
  SExpr.SUSym t -> ":" <> t
  where
    escapeStr = T.replace "\"" "\\\"" . T.replace "\\" "\\\\"

ignoreIO :: IO () -> IO ()
ignoreIO io = do
  _ <- try io :: IO (Either SomeException ())
  pure ()
