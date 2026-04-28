{-# LANGUAGE OverloadedStrings #-}

module ExampleSpec (spec) where

import Test.Hspec

import qualified Data.Map.Strict as M
import qualified Data.Text       as T
import qualified Data.Text.IO    as T.IO
import Control.Exception (ErrorCall(..), try, evaluate)
import Control.Monad (filterM)
import Data.List (sort, isInfixOf)
import System.Directory (listDirectory, doesFileExist, doesDirectoryExist)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeExtension, dropExtension, takeBaseName)
import System.Process (readProcessWithExitCode)

import qualified Pllisp.Codegen        as Codegen
import qualified Pllisp.ClosureConvert as CC
import qualified Pllisp.ExhaustCheck   as Exhaust
import qualified Pllisp.Type           as Ty
import qualified Pllisp.CST            as CST
import qualified Pllisp.LambdaLift     as LL
import qualified Pllisp.MacroExpand    as MacroExpand
import qualified Pllisp.Module         as Mod
import qualified Pllisp.Parser         as Parser
import qualified Pllisp.SExpr          as SExpr
import qualified Pllisp.Stdlib         as Stdlib
import qualified Pllisp.Resolve        as Resolve
import qualified Pllisp.TypeCheck      as TC

spec :: Spec
spec = do
  describe "valid examples" $ do
    files <- runIO $ findExamples "example-programs/valid"
    mapM_ validTest files

  describe "invalid examples" $ do
    files <- runIO $ findExamples "example-programs/invalid"
    mapM_ invalidTest files

  describe "module examples (valid)" $ do
    dirs <- runIO $ findModuleExamples "example-programs/modules/valid"
    mapM_ validModuleTest dirs

  describe "module examples (invalid)" $ do
    dirs <- runIO $ findModuleExamples "example-programs/modules/invalid"
    mapM_ invalidModuleTest dirs

findExamples :: FilePath -> IO [FilePath]
findExamples dir = do
  files <- listDirectory dir
  pure $ sort [dir </> f | f <- files, takeExtension f == ".pllisp"]

validTest :: FilePath -> Spec
validTest path = it (takeBaseName path) $ do
  src <- T.IO.readFile path
  let expectedFile = dropExtension path ++ ".expected"
  hasExpected <- doesFileExist expectedFile
  result <- run src
  if hasExpected
    then do
      expected <- readFile expectedFile
      result `shouldBe` strip expected
    else
      pure ()

invalidTest :: FilePath -> Spec
invalidTest path = it (takeBaseName path) $ do
  src <- T.IO.readFile path
  let errorFile = dropExtension path ++ ".error"
  hasError <- doesFileExist errorFile
  if hasError
    then do
      expected <- readFile errorFile
      shouldFailToCompile src (strip expected)
    else do
      result <- try (pipeline src >>= evaluate) :: IO (Either ErrorCall T.Text)
      case result of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected compilation to fail but it succeeded"

shouldFailToCompile :: T.Text -> String -> IO ()
shouldFailToCompile src msg = do
  result <- try (pipeline src >>= evaluate) :: IO (Either ErrorCall T.Text)
  case result of
    Left (ErrorCall e)
      | msg `isInfixOf` e -> pure ()
      | otherwise -> expectationFailure
          ("expected error containing " ++ show msg ++ " but got:\n" ++ e)
    Right _ -> expectationFailure
      ("expected compilation to fail with: " ++ msg ++ " but it succeeded")

strip :: String -> String
strip = reverse . dropWhile (== '\n') . reverse

-- Compilation and execution infrastructure (mirrors CodegenSpec but adds exhaust checking)

pipeline :: T.Text -> IO T.Text
pipeline = multiModulePipeline []

run :: T.Text -> IO String
run src = do
  ir <- pipeline src
  T.IO.writeFile "/tmp/pllisp_ex_test.ll" ir
  T.IO.writeFile "/tmp/pll_ex_ffi_bridge.c" Ty.ffiBridgeC
  (ec1, _, err1) <- readProcessWithExitCode
    "clang" ["/tmp/pllisp_ex_test.ll", "/tmp/pll_ex_ffi_bridge.c",
             "-o", "/tmp/pllisp_ex_test_exe", "-lm", "-lpcre2-8", "-lgc", "-lffi"] ""
  case ec1 of
    ExitFailure _ -> error ("clang failed:\n" ++ err1 ++ "\nIR:\n" ++ T.unpack ir)
    ExitSuccess -> do
      (ec2, out, err2) <- readProcessWithExitCode
        "/tmp/pllisp_ex_test_exe" [] ""
      case ec2 of
        ExitSuccess   -> pure (strip out)
        ExitFailure c -> error ("Program exited with " ++ show c ++ ":\n" ++ err2)

findModuleExamples :: FilePath -> IO [FilePath]
findModuleExamples dir = do
  exists <- doesDirectoryExist dir
  if not exists then pure []
  else do
    entries <- listDirectory dir
    dirs <- filterM (\e -> doesDirectoryExist (dir </> e)) entries
    pure $ sort [dir </> d | d <- dirs]

loadModuleExample :: FilePath -> IO ([(CST.Symbol, T.Text)], T.Text)
loadModuleExample dir = do
  files <- listDirectory dir
  let modFiles = [f | f <- files, takeExtension f == ".pllisp", f /= "main.pllisp"]
  mainSrc <- T.IO.readFile (dir </> "main.pllisp")
  mods <- mapM (\f -> do
    src <- T.IO.readFile (dir </> f)
    let name = T.toUpper (T.pack (takeBaseName f))
    pure (name, src)) modFiles
  pure (mods, mainSrc)

validModuleTest :: FilePath -> Spec
validModuleTest dir = it (takeBaseName dir) $ do
  (mods, mainSrc) <- loadModuleExample dir
  let expectedFile = dir </> "main.expected"
  hasExpected <- doesFileExist expectedFile
  result <- runModules mods mainSrc
  if hasExpected
    then do
      expected <- readFile expectedFile
      result `shouldBe` strip expected
    else pure ()

invalidModuleTest :: FilePath -> Spec
invalidModuleTest dir = it (takeBaseName dir) $ do
  (mods, mainSrc) <- loadModuleExample dir
  let errorFile = dir </> "main.error"
  hasError <- doesFileExist errorFile
  if hasError
    then do
      expected <- readFile errorFile
      shouldFailToCompileModules mods mainSrc (strip expected)
    else do
      result <- try (multiModulePipeline mods mainSrc >>= evaluate)
                  :: IO (Either ErrorCall T.Text)
      case result of
        Left _  -> pure ()
        Right _ -> expectationFailure "expected compilation to fail but it succeeded"

shouldFailToCompileModules :: [(CST.Symbol, T.Text)] -> T.Text -> String -> IO ()
shouldFailToCompileModules mods mainSrc msg = do
  result <- try (multiModulePipeline mods mainSrc >>= evaluate)
              :: IO (Either ErrorCall T.Text)
  case result of
    Left (ErrorCall e)
      | msg `isInfixOf` e -> pure ()
      | otherwise -> expectationFailure
          ("expected error containing " ++ show msg ++ " but got:\n" ++ e)
    Right _ -> expectationFailure
      ("expected compilation to fail with: " ++ msg ++ " but it succeeded")

runModules :: [(CST.Symbol, T.Text)] -> T.Text -> IO String
runModules mods mainSrc = do
  ir <- multiModulePipeline mods mainSrc
  T.IO.writeFile "/tmp/pllisp_mod_test.ll" ir
  T.IO.writeFile "/tmp/pll_mod_ffi_bridge.c" Ty.ffiBridgeC
  (ec1, _, err1) <- readProcessWithExitCode
    "clang" ["/tmp/pllisp_mod_test.ll", "/tmp/pll_mod_ffi_bridge.c",
             "-o", "/tmp/pllisp_mod_test_exe", "-lm", "-lpcre2-8", "-lgc", "-lffi"] ""
  case ec1 of
    ExitFailure _ -> error ("clang failed:\n" ++ err1 ++ "\nIR:\n" ++ T.unpack ir)
    ExitSuccess -> do
      (ec2, out, err2) <- readProcessWithExitCode
        "/tmp/pllisp_mod_test_exe" [] ""
      case ec2 of
        ExitSuccess   -> pure (strip out)
        ExitFailure c -> error ("Program exited with " ++ show c ++ ":\n" ++ err2)

multiModulePipeline :: [(CST.Symbol, T.Text)] -> T.Text -> IO T.Text
multiModulePipeline modules mainSrc = do
  preludeSexprs <- Stdlib.loadPrelude
  let parsedModules = [(name, parseMod name src) | (name, src) <- modules]
      allModules = ("PRELUDE", preludeSexprs) : parsedModules
      modMap = M.fromList allModules
      rawDepMap = M.map (map CST.impModule . SExpr.preScanImports) modMap
      depMap = M.mapWithKey (\k ds ->
        if k == "PRELUDE" || "PRELUDE" `elem` ds then ds
        else "PRELUDE" : ds) rawDepMap
  order <- case Mod.dependencyOrder depMap of
    Left e  -> error ("dep order: " ++ e)
    Right o -> pure o
  let (finalExports, finalTyped, finalMacros, finalEnvs) =
        foldl (compileOneMod modMap) (M.empty, [], [], TC.emptyTCEnvs) order
      mainSexprs = parseMod "MAIN" mainSrc
      mainExpanded = case MacroExpand.expandWith finalMacros mainSexprs of
        Left e  -> error ("main macro: " ++ e)
        Right s -> s
      mainProg = case SExpr.toProgram mainExpanded of
        Left e  -> error ("main sexpr: " ++ SExpr.ceMsg e)
        Right p -> p
      preludeExports = M.findWithDefault M.empty "PRELUDE" finalExports
      preludeImport = CST.Import "PRELUDE" "PRELUDE" (M.keys preludeExports)
      allMainImports = preludeImport : CST.progImports mainProg
  case Mod.checkImportCollisions finalExports allMainImports of
    Left e  -> error ("main collision: " ++ e)
    Right () -> pure ()
  let (rScope, tcCtx, nMap) = Mod.buildImportScope finalExports allMainImports
  mainExprs <- case Mod.desugarTopLevel (CST.progExprs mainProg) of
    Left e  -> error ("main desugar: " ++ e)
    Right e -> pure e
  case Resolve.resolveWith rScope nMap mainExprs of
    Left e  -> error ("main resolve: " ++ show e)
    Right resolved -> case TC.typecheckWith finalEnvs tcCtx resolved of
      Left e  -> error ("main typecheck: " ++ show e)
      Right (typed, _) ->
        let merged = Mod.mergeImportedCode finalTyped typed
        in case Exhaust.exhaustCheck merged of
          errs@(_:_) -> error ("exhaust: " ++ unwords (map Exhaust.exhaMsg errs))
          [] -> pure $ Codegen.codegen (LL.lambdaLift (CC.closureConvert merged))
  where
    parseMod name src = case Parser.parseSExprs "<mod>" src of
      Left e  -> error ("parse " ++ T.unpack name ++ ": " ++ show e)
      Right s -> s

    compileOneMod modMap (accExports, accTyped, accMacros, accEnvs) modName =
      let sexprs = modMap M.! modName
          thisMacros = MacroExpand.extractMacroDefs sexprs
          isPrelude = modName == "PRELUDE"
          expanded = case MacroExpand.expandWith accMacros sexprs of
            Left e  -> error ("macro " ++ T.unpack modName ++ ": " ++ e)
            Right s -> s
          modProg = case SExpr.toProgram expanded of
            Left e  -> error ("sexpr " ++ T.unpack modName ++ ": " ++ SExpr.ceMsg e)
            Right p -> p
          preludeExports = M.findWithDefault M.empty "PRELUDE" accExports
          cstImports = CST.progImports modProg
          allImports = if isPrelude then cstImports
                       else CST.Import "PRELUDE" "PRELUDE" (M.keys preludeExports) : cstImports
          (rScope, tcCtx, nMap) = case Mod.checkImportCollisions accExports allImports of
            Left e  -> error ("collision " ++ T.unpack modName ++ ": " ++ e)
            Right () -> Mod.buildImportScope accExports allImports
          exprs = case Mod.desugarTopLevel (CST.progExprs modProg) of
            Left e  -> error ("desugar " ++ T.unpack modName ++ ": " ++ e)
            Right e -> e
          (typed, modEnvs) = case Resolve.resolveWith rScope nMap exprs of
            Left e  -> error ("resolve " ++ T.unpack modName ++ ": " ++ show e)
            Right resolved -> case TC.typecheckWith accEnvs tcCtx resolved of
              Left e  -> error ("tc " ++ T.unpack modName ++ ": " ++ show e)
              Right r -> r
          modExports = Mod.collectExports modEnvs typed
      in (M.insert modName modExports accExports,
          accTyped ++ [typed],
          accMacros ++ thisMacros,
          modEnvs)
