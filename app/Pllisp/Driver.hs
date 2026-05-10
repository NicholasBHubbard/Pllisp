{-# LANGUAGE OverloadedStrings #-}

module Pllisp.Driver (runFiles, compileFile) where

import System.Directory (removeFile)
import System.Exit (ExitCode(..))
import System.FilePath (dropExtension, takeDirectory)
import System.Process (readProcessWithExitCode)

import qualified Data.Map.Strict as M
import qualified Data.Set as S
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
import qualified Pllisp.Pipeline as Pipeline
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
      importResult <- Pipeline.loadImports fp stdlibDir macroImports
      case importResult of
        Left err -> putStrLn err >> pure (ExitFailure 1)
        Right loaded ->
          case Pipeline.compileBaseState isPrelude macroImports (Pipeline.liCompileStates loaded) of
            Left err -> putStrLn err >> pure (ExitFailure 1)
            Right baseState ->
              case MacroExpand.expandModuleWith (Pipeline.moduleNameOrUser sexprs) baseState sexprs of
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
  -> Pipeline.LoadedImports
  -> CST.Program
  -> IO ExitCode
compileExpandedProgram fp _ render loaded prog = do
  let isPrelude = CST.progName prog == Just "PRELUDE"
      explicitImports = CST.progImports prog
      preludeExports = M.findWithDefault M.empty "PRELUDE" (Pipeline.liExports loaded)
      preludeMacroNames = case M.lookup "PRELUDE" (Pipeline.liCompileStates loaded) of
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
      case Mod.checkImportCollisions (Pipeline.liExports loaded) fixedImports of
        Left err -> putStrLn err >> pure (ExitFailure 1)
        Right () -> do
          let (resolveScope, tcCtx, normMap) = Mod.buildImportScope (Pipeline.liExports loaded) fixedImports
          case Mod.desugarTopLevel (CST.progExprs prog) of
            Left err -> putStrLn ("desugar error: " ++ err) >> pure (ExitFailure 1)
            Right exprs -> case Resolve.resolveWith resolveScope normMap exprs of
              Left errs -> do
                mapM_ (\e -> render "resolve" (Resolve.errSpan e) (Resolve.errMsg e)) errs
                pure (ExitFailure 1)
              Right resolved ->
                case TC.typecheckWith (Pipeline.liEnvs loaded) tcCtx resolved of
                  Left errs -> do
                    mapM_ (\e -> render "type" (TC.teSpan e) (TC.teMsg e)) errs
                    pure (ExitFailure 1)
                  Right (typed, _) -> do
                    let merged = Mod.mergeImportedCode (Pipeline.liTypedModules loaded) typed
                    case Pipeline.validateRuntimeSyntaxTypes merged of
                      Just (sp, msg) -> render "type" sp msg >> pure (ExitFailure 1)
                      Nothing ->
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
