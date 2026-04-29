{-# LANGUAGE OverloadedStrings #-}

module Pllisp.Driver (runFiles, compileFile) where

import System.Directory (doesFileExist, removeFile)
import System.Exit (ExitCode(..))
import System.FilePath (dropExtension, takeDirectory, (</>))
import System.Process (readProcessWithExitCode)

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import qualified Text.Megaparsec as MP

import qualified Pllisp.CST as CST
import qualified Pllisp.Codegen as Codegen
import qualified Pllisp.ClosureConvert as CC
import qualified Pllisp.Error as Error
import qualified Pllisp.ExhaustCheck as Exhaust
import qualified Pllisp.LambdaLift as LL
import qualified Pllisp.MacroExpand as MacroExpand
import qualified Pllisp.Module as Mod
import qualified Pllisp.Parser as Parser
import qualified Pllisp.Resolve as Resolve
import qualified Pllisp.SExpr as SExpr
import qualified Pllisp.SrcLoc as Loc
import qualified Pllisp.Stdlib as Stdlib
import qualified Pllisp.Type as Ty
import qualified Pllisp.TypeCheck as TC

runFiles :: [FilePath] -> IO ExitCode
runFiles fps = do
  results <- mapM compileFile fps
  pure $
    if all (== ExitSuccess) results
      then ExitSuccess
      else ExitFailure 1

compileFile :: FilePath -> IO ExitCode
compileFile fp = do
  src <- T.IO.readFile fp
  stdlibDir <- Stdlib.getStdlibDirNear [takeDirectory fp]
  let render kind sp msg = putStr (Error.renderError src kind sp msg)
  case Parser.parseSExprs fp src of
    Left err -> putStr (MP.errorBundlePretty err) >> pure (ExitFailure 1)
    Right sexprs -> do
      let isPrelude = SExpr.preScanModuleName sexprs == Just "PRELUDE"
          explicitImports = SExpr.preScanImports sexprs
          macroImports = if isPrelude then explicitImports
                         else CST.Import "PRELUDE" "PRELUDE" [] : explicitImports
      importResult <- loadImports fp stdlibDir macroImports
      case importResult of
        Left err -> putStrLn err >> pure (ExitFailure 1)
        Right loaded ->
          case compileBaseState isPrelude macroImports (liCompileStates loaded) of
            Left err -> putStrLn err >> pure (ExitFailure 1)
            Right baseState ->
              case MacroExpand.expandModuleWith (moduleNameOrUser sexprs) baseState sexprs of
                Left err -> putStrLn ("macro error: " ++ err) >> pure (ExitFailure 1)
                Right result ->
                  case SExpr.toProgram (MacroExpand.mrExpanded result) of
                    Left err -> putStrLn ("syntax error: " ++ SExpr.ceMsg err) >> pure (ExitFailure 1)
                    Right prog ->
                      case CST.progName prog of
                        Just name -> case Mod.validateModuleName name fp of
                          Just err -> putStrLn err >> pure (ExitFailure 1)
                          Nothing -> compileExpandedProgram fp stdlibDir render loaded prog
                        Nothing -> compileExpandedProgram fp stdlibDir render loaded prog

compileExpandedProgram
  :: FilePath
  -> FilePath
  -> (String -> Loc.Span -> String -> IO ())
  -> LoadedImports
  -> CST.Program
  -> IO ExitCode
compileExpandedProgram fp _ render loaded prog = do
  let isPrelude = CST.progName prog == Just "PRELUDE"
      explicitImports = CST.progImports prog
      preludeExports = M.findWithDefault M.empty "PRELUDE" (liExports loaded)
      preludeMacroNames = case M.lookup "PRELUDE" (liCompileStates loaded) of
        Just st -> M.keysSet (MacroExpand.csMacros st)
        Nothing -> S.empty
      protectedNames =
        if isPrelude
          then S.empty
          else M.keysSet preludeExports `S.union` preludeMacroNames
      fixedImports =
        if isPrelude
          then explicitImports
          else CST.Import "PRELUDE" "PRELUDE" (M.keys preludeExports) : explicitImports
  case Mod.validateProgramNames protectedNames (CST.progExprs prog) of
    Left err -> putStrLn err >> pure (ExitFailure 1)
    Right () ->
      case Mod.checkImportCollisions (liExports loaded) fixedImports of
        Left err -> putStrLn err >> pure (ExitFailure 1)
        Right () -> do
          let (resolveScope, tcCtx, normMap) = Mod.buildImportScope (liExports loaded) fixedImports
          case Mod.desugarTopLevel (CST.progExprs prog) of
            Left err -> putStrLn ("desugar error: " ++ err) >> pure (ExitFailure 1)
            Right exprs -> case Resolve.resolveWith resolveScope normMap exprs of
              Left errs -> do
                mapM_ (\e -> render "resolve" (Resolve.errSpan e) (Resolve.errMsg e)) errs
                pure (ExitFailure 1)
              Right resolved ->
                case TC.typecheckWith (liEnvs loaded) tcCtx resolved of
                  Left errs -> do
                    mapM_ (\e -> render "type" (TC.teSpan e) (TC.teMsg e)) errs
                    pure (ExitFailure 1)
                  Right (typed, _) -> do
                    let merged = Mod.mergeImportedCode (liTypedModules loaded) typed
                    case Exhaust.exhaustCheck merged of
                      errs@(_:_) -> do
                        mapM_ (\e -> render "exhaust" (Exhaust.exhaSpan e) (Exhaust.exhaMsg e)) errs
                        pure (ExitFailure 1)
                      [] -> do
                        let ir = Codegen.codegen (LL.lambdaLift (CC.closureConvert merged))
                            base = dropExtension fp
                            llFile = base ++ ".ll"
                            bridgeFile = base ++ "_ffi_bridge.c"
                            exeFile = base
                        T.IO.writeFile llFile ir
                        T.IO.writeFile bridgeFile Ty.ffiBridgeC
                        (ec, _, err') <- readProcessWithExitCode
                          "clang" [llFile, bridgeFile, "-o", exeFile,
                                   "-lm", "-lpcre2-8", "-lgc", "-lffi"] ""
                        removeFile bridgeFile
                        case ec of
                          ExitFailure _ -> do
                            putStrLn ("clang failed:\n" ++ err')
                            pure (ExitFailure 1)
                          ExitSuccess -> do
                            putStrLn ("compiled: " ++ exeFile)
                            pure ExitSuccess

data ModuleInfo = ModuleInfo
  { miPath :: FilePath
  , miSexprs :: [SExpr.SExpr]
  , miImports :: [CST.Import]
  }

data LoadedImports = LoadedImports
  { liExports :: M.Map CST.Symbol (M.Map CST.Symbol TC.Scheme)
  , liTypedModules :: [TC.TResolvedCST]
  , liEnvs :: TC.TCEnvs
  , liCompileStates :: M.Map CST.Symbol MacroExpand.CompileState
  }

loadImports :: FilePath -> FilePath -> [CST.Import] -> IO (Either String LoadedImports)
loadImports _ _ [] =
  pure $ Right $
    LoadedImports
      { liExports = M.empty
      , liTypedModules = []
      , liEnvs = TC.emptyTCEnvs
      , liCompileStates = M.empty
      }
loadImports fp stdlibDir imports = do
  let searchDir = takeDirectory fp
  scanResult <- scanAllModules searchDir stdlibDir imports
  case scanResult of
    Left err -> pure (Left err)
    Right moduleInfos -> do
      let rawDepMap = M.map (map CST.impModule . miImports) moduleInfos
          depMap = M.mapWithKey (\k ds ->
            if k == "PRELUDE" || "PRELUDE" `elem` ds then ds
            else "PRELUDE" : ds) rawDepMap
      case Mod.dependencyOrder depMap of
        Left err -> pure (Left err)
        Right order ->
          case expandModules moduleInfos order of
            Left err -> pure (Left err)
            Right (expandedMap, compileStates) ->
              compileModules expandedMap compileStates moduleInfos order
                M.empty [] TC.emptyTCEnvs

scanAllModules :: FilePath -> FilePath -> [CST.Import] -> IO (Either String (M.Map CST.Symbol ModuleInfo))
scanAllModules searchDir stdlibDir rootImports =
  go S.empty M.empty [(imp, searchDir) | imp <- rootImports]
  where
    go _ acc [] = pure (Right acc)
    go visited acc ((CST.Import modName _ _, dir) : rest)
      | S.member modName visited = go visited acc rest
      | otherwise = do
          mPath <- findModuleFile dir stdlibDir modName
          case mPath of
            Nothing -> pure (Left ("module not found: " ++ T.unpack modName))
            Just modPath -> do
              src <- T.IO.readFile modPath
              case Parser.parseSExprs modPath src of
                Left err -> pure (Left ("parse error in " ++ T.unpack modName
                  ++ ": " ++ MP.errorBundlePretty err))
                Right sexprs -> do
                  let subImports = SExpr.preScanImports sexprs
                      info = ModuleInfo modPath sexprs subImports
                      visited' = S.insert modName visited
                      acc' = M.insert modName info acc
                      modDir = takeDirectory modPath
                      newItems = [(imp, modDir) | imp <- subImports]
                  go visited' acc' (rest ++ newItems)

expandModules
  :: M.Map CST.Symbol ModuleInfo
  -> [CST.Symbol]
  -> Either String (M.Map CST.Symbol [SExpr.SExpr], M.Map CST.Symbol MacroExpand.CompileState)
expandModules moduleInfos = go M.empty M.empty
  where
    go expandedMap compileStates [] = Right (expandedMap, compileStates)
    go expandedMap compileStates (modName : rest) =
      case M.lookup modName moduleInfos of
        Nothing -> Left ("missing module info for " ++ T.unpack modName)
        Just info ->
          let isPrelude = modName == "PRELUDE"
              macroImports = if isPrelude then miImports info
                             else CST.Import "PRELUDE" "PRELUDE" [] : miImports info
          in case compileBaseState isPrelude macroImports compileStates of
               Left err -> Left err
               Right baseState ->
                 case MacroExpand.expandModuleWith modName baseState (miSexprs info) of
                   Left err -> Left ("macro error in " ++ T.unpack modName ++ ": " ++ err)
                   Right result ->
                     go (M.insert modName (MacroExpand.mrExpanded result) expandedMap)
                        (M.insert modName (MacroExpand.mrState result) compileStates)
                        rest

compileBaseState
  :: Bool
  -> [CST.Import]
  -> M.Map CST.Symbol MacroExpand.CompileState
  -> Either String MacroExpand.CompileState
compileBaseState True _ _ = Right MacroExpand.primitiveState
compileBaseState False imports compileStates =
  let names = map CST.impModule imports
  in do
    states <- mapM lookupState names
    MacroExpand.mergeCompileStates states
  where
    lookupState name =
      case M.lookup name compileStates of
        Just st -> Right st
        Nothing -> Left ("missing compile-time state for " ++ T.unpack name)

compileModules
  :: M.Map CST.Symbol [SExpr.SExpr]
  -> M.Map CST.Symbol MacroExpand.CompileState
  -> M.Map CST.Symbol ModuleInfo
  -> [CST.Symbol]
  -> M.Map CST.Symbol (M.Map CST.Symbol TC.Scheme)
  -> [TC.TResolvedCST]
  -> TC.TCEnvs
  -> IO (Either String LoadedImports)
compileModules _ compileStates _ [] accExports accTyped accEnvs =
  pure $ Right $
    LoadedImports
      { liExports = accExports
      , liTypedModules = accTyped
      , liEnvs = accEnvs
      , liCompileStates = compileStates
      }
compileModules expandedMap compileStates moduleInfos (modName : rest) accExports accTyped accEnvs =
  case (M.lookup modName moduleInfos, M.lookup modName expandedMap) of
    (Nothing, _) -> compileModules expandedMap compileStates moduleInfos rest accExports accTyped accEnvs
    (_, Nothing) -> pure (Left ("missing expanded module for " ++ T.unpack modName))
    (Just info, Just expanded) ->
      case SExpr.toProgram expanded of
        Left err -> pure (Left ("syntax error in " ++ T.unpack modName
          ++ ": " ++ SExpr.ceMsg err))
        Right modProg -> do
          let cstImports = CST.progImports modProg
              isPrelude = modName == "PRELUDE"
              preludeExports = M.findWithDefault M.empty "PRELUDE" accExports
              preludeMacroNames = case M.lookup "PRELUDE" compileStates of
                Just st -> M.keysSet (MacroExpand.csMacros st)
                Nothing -> S.empty
              protectedNames =
                if isPrelude
                  then S.empty
                  else M.keysSet preludeExports `S.union` preludeMacroNames
              allImports = if isPrelude then cstImports
                           else CST.Import "PRELUDE" "PRELUDE" (M.keys preludeExports) : cstImports
          case Mod.validateProgramNames protectedNames (CST.progExprs modProg) of
            Left err -> pure (Left err)
            Right () ->
              case Mod.checkImportCollisions accExports allImports of
                Left err -> pure (Left err)
                Right () -> do
                  let (resolveScope, tcCtx, normMap) =
                        Mod.buildImportScope accExports allImports
                  case Mod.desugarTopLevel (CST.progExprs modProg) of
                    Left err -> pure (Left ("desugar error in " ++ T.unpack modName ++ ": " ++ err))
                    Right exprs -> case Resolve.resolveWith resolveScope normMap exprs of
                      Left errs -> do
                        src <- T.IO.readFile (miPath info)
                        pure (Left (concatMap (\e ->
                          Error.renderError src "resolve" (Resolve.errSpan e) (Resolve.errMsg e)) errs))
                      Right resolved -> case TC.typecheckWith accEnvs tcCtx resolved of
                        Left errs -> do
                          src <- T.IO.readFile (miPath info)
                          pure (Left (concatMap (\e ->
                            Error.renderError src "type" (TC.teSpan e) (TC.teMsg e)) errs))
                        Right (typed, modEnvs) -> do
                          let exports = Mod.collectExports modEnvs typed
                              accExports' = M.insert modName exports accExports
                          compileModules expandedMap compileStates moduleInfos rest
                            accExports' (accTyped ++ [typed]) modEnvs

findModuleFile :: FilePath -> FilePath -> CST.Symbol -> IO (Maybe FilePath)
findModuleFile searchDir stdlibDir modName = do
  let localPath = searchDir </> T.unpack modName ++ ".pll"
  localExists <- doesFileExist localPath
  if localExists then pure (Just localPath)
  else do
    let stdlibPath = stdlibDir </> T.unpack modName ++ ".pll"
    stdlibExists <- doesFileExist stdlibPath
    pure (if stdlibExists then Just stdlibPath else Nothing)

moduleNameOrUser :: [SExpr.SExpr] -> T.Text
moduleNameOrUser sexprs = maybe "USER" id (SExpr.preScanModuleName sexprs)
