{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitFailure)
import System.FilePath (takeDirectory, dropExtension, (</>))
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
import qualified Pllisp.SExpr          as SExpr
import qualified Pllisp.Stdlib         as Stdlib
import qualified Pllisp.Resolve        as Resolve
import qualified Pllisp.SrcLoc         as Loc
import qualified Pllisp.Type           as Ty
import qualified Pllisp.TypeCheck      as TC

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "usage: pllisp <file> [<file>...]"
    _  -> mapM_ compileFile args

compileFile :: FilePath -> IO ()
compileFile fp = do
  src <- T.IO.readFile fp
  let render kind sp msg = putStr (Error.renderError src kind sp msg)
  case Parser.parseSExprs fp src of
    Left err -> putStr (MP.errorBundlePretty err)
    Right sexprs ->
      case MacroExpand.expand sexprs of
        Left err -> putStrLn ("macro error: " ++ err)
        Right expanded ->
          case SExpr.toProgram expanded of
            Left err -> putStrLn ("syntax error: " ++ SExpr.ceMsg err)
            Right prog ->
              case CST.progName prog of
                Just name -> case Mod.validateModuleName name fp of
                  Just err -> putStrLn err >> pure ()
                  Nothing  -> compileProg fp src render prog
                Nothing -> compileProg fp src render prog

compileProg :: FilePath -> T.Text -> (String -> Loc.Span -> String -> IO ()) -> CST.Program -> IO ()
compileProg fp src render prog = do
  importResult <- loadImports fp (CST.progImports prog)
  case importResult of
    Left err -> putStrLn err
    Right (exportMap, importedTypedModules) -> do
      preludeDecls <- Stdlib.loadPrelude
      let (resolveScope, tcCtx, normMap) = Mod.buildImportScope exportMap (CST.progImports prog)
          exprs = preludeDecls ++ Mod.desugarTopLevel (CST.progExprs prog)
      case Resolve.resolveWith resolveScope normMap exprs of
        Left errs -> mapM_ (\e -> render "resolve" (Resolve.errSpan e) (Resolve.errMsg e)) errs
        Right resolved ->
          case TC.typecheck tcCtx resolved of
            Left errs -> mapM_ (\e -> render "type" (TC.teSpan e) (TC.teMsg e)) errs
            Right typed ->
              case Exhaust.exhaustCheck typed of
                errs@(_:_) -> mapM_ (\e -> render "exhaust" (Exhaust.exhaSpan e) (Exhaust.exhaMsg e)) errs
                [] -> do
                  let merged = Mod.mergeImportedCode importedTypedModules typed
                      ir = Codegen.codegen (LL.lambdaLift (CC.closureConvert merged))
                      base = dropExtension fp
                      llFile = base ++ ".ll"
                      bridgeFile = base ++ "_ffi_bridge.c"
                      exeFile = base
                  T.IO.writeFile llFile ir
                  T.IO.writeFile bridgeFile Ty.ffiBridgeC
                  (ec, _, err') <- readProcessWithExitCode
                    "clang" [llFile, bridgeFile, "-o", exeFile,
                             "-lm", "-lpcre2-8", "-lffi"] ""
                  removeFile bridgeFile
                  case ec of
                    ExitFailure _ -> do
                      putStrLn ("clang failed:\n" ++ err')
                      exitFailure
                    ExitSuccess ->
                      putStrLn ("compiled: " ++ exeFile)

-- | Load all imported modules, returning their export maps and typed ASTs.
loadImports :: FilePath -> [CST.Import]
  -> IO (Either String (M.Map CST.Symbol (M.Map CST.Symbol TC.Scheme), [TC.TResolvedCST]))
loadImports _ [] = pure (Right (M.empty, []))
loadImports fp imports = do
  let searchDir = takeDirectory fp
  results <- mapM (loadModule searchDir) imports
  case sequence results of
    Left err -> pure (Left err)
    Right triples ->
      let exportMap = M.fromList [(n, e) | (n, e, _) <- triples]
          typedMods = [t | (_, _, t) <- triples]
      in pure (Right (exportMap, typedMods))

loadModule :: FilePath -> CST.Import
  -> IO (Either String (CST.Symbol, M.Map CST.Symbol TC.Scheme, TC.TResolvedCST))
loadModule searchDir imp = do
  let modName = CST.impModule imp
      modFile = searchDir </> T.unpack modName ++ ".pll"
  exists <- doesFileExist modFile
  if not exists
    then pure (Left ("module not found: " ++ T.unpack modName ++ " (looked for " ++ modFile ++ ")"))
    else do
      src <- T.IO.readFile modFile
      case Parser.parseSExprs modFile src of
        Left err -> pure (Left ("parse error in module " ++ T.unpack modName ++ ": " ++ MP.errorBundlePretty err))
        Right sexprs -> case MacroExpand.expand sexprs of
          Left err -> pure (Left ("macro error in module " ++ T.unpack modName ++ ": " ++ err))
          Right expanded -> case SExpr.toProgram expanded of
            Left err -> pure (Left ("syntax error in module " ++ T.unpack modName ++ ": " ++ SExpr.ceMsg err))
            Right modProg -> do
              preludeDecls <- Stdlib.loadPrelude
              case Resolve.resolve S.empty (preludeDecls ++ Mod.desugarTopLevel (CST.progExprs modProg)) of
                Left _ -> pure (Left ("resolve error in module " ++ T.unpack modName))
                Right resolved -> case TC.typecheck M.empty resolved of
                  Left _ -> pure (Left ("type error in module " ++ T.unpack modName))
                  Right typed ->
                    let exports = Mod.collectExports typed
                    in pure (Right (modName, exports, typed))
