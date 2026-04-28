{-# LANGUAGE OverloadedStrings #-}

module Pllisp.Driver (runFiles, compileFile) where

import System.Exit (ExitCode(..))
import System.FilePath (dropExtension, takeDirectory, (</>))
import System.Directory (doesFileExist, removeFile)
import System.Process (readProcessWithExitCode)

import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import qualified Data.Text       as T
import qualified Data.Text.IO    as T.IO
import qualified Text.Megaparsec as MP

import qualified Pllisp.Codegen        as Codegen
import qualified Pllisp.ClosureConvert as CC
import qualified Pllisp.CST            as CST
import qualified Pllisp.Error          as Error
import qualified Pllisp.ExhaustCheck   as Exhaust
import qualified Pllisp.LambdaLift     as LL
import qualified Pllisp.MacroExpand    as MacroExpand
import qualified Pllisp.Module         as Mod
import qualified Pllisp.Parser         as Parser
import qualified Pllisp.Resolve        as Resolve
import qualified Pllisp.SExpr          as SExpr
import qualified Pllisp.SrcLoc         as Loc
import qualified Pllisp.Stdlib         as Stdlib
import qualified Pllisp.Type           as Ty
import qualified Pllisp.TypeCheck      as TC

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
      searchDir = takeDirectory fp
  case Parser.parseSExprs fp src of
    Left err -> putStr (MP.errorBundlePretty err) >> pure (ExitFailure 1)
    Right sexprs -> do
      let isPrelude = SExpr.preScanModuleName sexprs == Just "PRELUDE"
          imports = SExpr.preScanImports sexprs
          macroImports = if isPrelude then imports
                         else CST.Import "PRELUDE" "PRELUDE" [] : imports
      macroResult <- loadImportedMacros S.empty searchDir stdlibDir macroImports
      case macroResult of
        Left err -> putStrLn ("import error: " ++ err) >> pure (ExitFailure 1)
        Right importedMacros ->
          case MacroExpand.expandWith importedMacros sexprs of
            Left err -> putStrLn ("macro error: " ++ err) >> pure (ExitFailure 1)
            Right expanded ->
              case SExpr.toProgram expanded of
                Left err -> putStrLn ("syntax error: " ++ SExpr.ceMsg err) >> pure (ExitFailure 1)
                Right prog ->
                  case CST.progName prog of
                    Just name -> case Mod.validateModuleName name fp of
                      Just err -> putStrLn err >> pure (ExitFailure 1)
                      Nothing  -> compileProg fp stdlibDir render prog
                    Nothing -> compileProg fp stdlibDir render prog

compileProg :: FilePath -> FilePath -> (String -> Loc.Span -> String -> IO ()) -> CST.Program -> IO ExitCode
compileProg fp stdlibDir render prog = do
  let isPrelude = CST.progName prog == Just "PRELUDE"
      explicitImports = CST.progImports prog
      allImports = if isPrelude then explicitImports
                   else CST.Import "PRELUDE" "PRELUDE" [] : explicitImports
  preludeMacroNamesResult <-
    if isPrelude
      then pure (Right S.empty)
      else loadPreludeMacroNames (takeDirectory fp) stdlibDir
  importResult <- loadImports fp stdlibDir allImports
  case (preludeMacroNamesResult, importResult) of
    (Left err, _) -> putStrLn err >> pure (ExitFailure 1)
    (_, Left err) -> putStrLn err >> pure (ExitFailure 1)
    (Right preludeMacroNames, Right (exportMap, importedTypedModules, importedEnvs)) -> do
      let preludeExports = M.findWithDefault M.empty "PRELUDE" exportMap
          protectedNames = if isPrelude then S.empty else M.keysSet preludeExports `S.union` preludeMacroNames
          fixedImports = if isPrelude then explicitImports
                         else CST.Import "PRELUDE" "PRELUDE" (M.keys preludeExports) : explicitImports
      case Mod.validateProgramNames protectedNames (CST.progExprs prog) of
        Left err -> putStrLn err >> pure (ExitFailure 1)
        Right () ->
          case Mod.checkImportCollisions exportMap fixedImports of
            Left err -> putStrLn err >> pure (ExitFailure 1)
            Right () -> do
              let (resolveScope, tcCtx, normMap) = Mod.buildImportScope exportMap fixedImports
              case Mod.desugarTopLevel (CST.progExprs prog) of
                Left err -> putStrLn ("desugar error: " ++ err) >> pure (ExitFailure 1)
                Right exprs -> case Resolve.resolveWith resolveScope normMap exprs of
                  Left errs -> do
                    mapM_ (\e -> render "resolve" (Resolve.errSpan e) (Resolve.errMsg e)) errs
                    pure (ExitFailure 1)
                  Right resolved ->
                    case TC.typecheckWith importedEnvs tcCtx resolved of
                      Left errs -> do
                        mapM_ (\e -> render "type" (TC.teSpan e) (TC.teMsg e)) errs
                        pure (ExitFailure 1)
                      Right (typed, _) -> do
                        let merged = Mod.mergeImportedCode importedTypedModules typed
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
  { miPath    :: FilePath
  , miSexprs  :: [SExpr.SExpr]
  , miImports :: [CST.Import]
  }

loadImports :: FilePath -> FilePath -> [CST.Import]
  -> IO (Either String (M.Map CST.Symbol (M.Map CST.Symbol TC.Scheme), [TC.TResolvedCST], TC.TCEnvs))
loadImports _ _ [] = pure (Right (M.empty, [], TC.emptyTCEnvs))
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
          compileModules stdlibDir moduleInfos order M.empty [] TC.emptyTCEnvs

scanAllModules :: FilePath -> FilePath -> [CST.Import]
  -> IO (Either String (M.Map CST.Symbol ModuleInfo))
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

compileModules :: FilePath -> M.Map CST.Symbol ModuleInfo
  -> [CST.Symbol] -> M.Map CST.Symbol (M.Map CST.Symbol TC.Scheme) -> [TC.TResolvedCST] -> TC.TCEnvs
  -> IO (Either String (M.Map CST.Symbol (M.Map CST.Symbol TC.Scheme), [TC.TResolvedCST], TC.TCEnvs))
compileModules _ _ [] accExports accTyped accEnvs = pure (Right (accExports, accTyped, accEnvs))
compileModules stdlibDir moduleInfos (modName : rest) accExports accTyped accEnvs =
  case M.lookup modName moduleInfos of
    Nothing -> compileModules stdlibDir moduleInfos rest accExports accTyped accEnvs
    Just info -> do
      let sexprs = miSexprs info
          modImports = miImports info
          isPrelude = modName == "PRELUDE"
          macroImports = if isPrelude then modImports
                         else CST.Import "PRELUDE" "PRELUDE" [] : modImports
      preludeMacroNamesResult <-
        if isPrelude
          then pure (Right S.empty)
          else loadPreludeMacroNames (takeDirectory (miPath info)) stdlibDir
      macroResult <- loadImportedMacros (S.singleton modName)
        (takeDirectory (miPath info)) stdlibDir macroImports
      case (preludeMacroNamesResult, macroResult) of
        (Left err, _) -> pure (Left err)
        (_, Left err) -> pure (Left err)
        (Right preludeMacroNames, Right importedMacros) ->
          case MacroExpand.expandWith importedMacros sexprs of
            Left err -> pure (Left ("macro error in " ++ T.unpack modName ++ ": " ++ err))
            Right expanded -> case SExpr.toProgram expanded of
              Left err -> pure (Left ("syntax error in " ++ T.unpack modName
                ++ ": " ++ SExpr.ceMsg err))
              Right modProg -> do
                let cstImports = CST.progImports modProg
                    preludeExports = M.findWithDefault M.empty "PRELUDE" accExports
                    protectedNames = if isPrelude then S.empty else M.keysSet preludeExports `S.union` preludeMacroNames
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
                                compileModules stdlibDir moduleInfos rest
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

loadImportedMacros :: S.Set CST.Symbol -> FilePath -> FilePath -> [CST.Import]
  -> IO (Either String [SExpr.SExpr])
loadImportedMacros _ _ _ [] = pure (Right [])
loadImportedMacros visited searchDir stdlibDir imports = do
  results <- mapM (loadModuleMacros visited searchDir stdlibDir) imports
  case sequence results of
    Left err -> pure (Left err)
    Right macroLists -> pure (Right (concat macroLists))

loadModuleMacros :: S.Set CST.Symbol -> FilePath -> FilePath -> CST.Import
  -> IO (Either String [SExpr.SExpr])
loadModuleMacros visited searchDir stdlibDir imp
  | S.member modName visited =
      pure (Left ("circular import involving " ++ T.unpack modName))
  | otherwise = do
      mPath <- findModuleFile searchDir stdlibDir modName
      case mPath of
        Nothing -> pure (Right [])
        Just modPath -> do
          src <- T.IO.readFile modPath
          case Parser.parseSExprs modPath src of
            Left _ -> pure (Right [])
            Right sexprs -> do
              let subImports = SExpr.preScanImports sexprs
                  visited' = S.insert modName visited
              subResult <- loadImportedMacros visited' (takeDirectory modPath) stdlibDir subImports
              case subResult of
                Left err -> pure (Left err)
                Right subMacros ->
                  let thisMacros = MacroExpand.extractMacroDefs sexprs
                  in pure (Right (subMacros ++ thisMacros))
  where modName = CST.impModule imp

loadPreludeMacroNames :: FilePath -> FilePath -> IO (Either String (S.Set CST.Symbol))
loadPreludeMacroNames searchDir stdlibDir = do
  result <- loadModuleMacros S.empty searchDir stdlibDir (CST.Import "PRELUDE" "PRELUDE" [])
  pure (fmap macroNames result)

macroNames :: [SExpr.SExpr] -> S.Set CST.Symbol
macroNames = go S.empty
  where
    go acc [] = acc
    go acc (Loc.Located _ (SExpr.SList (Loc.Located _ (SExpr.SAtom "MAC") : Loc.Located _ (SExpr.SAtom name) : _)) : rest) =
      go (S.insert name acc) rest
    go acc (_ : rest) = go acc rest
