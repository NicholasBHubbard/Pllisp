{-# LANGUAGE OverloadedStrings #-}

module ExampleSpec (spec) where

import Test.Hspec

import Control.Exception (IOException, bracket, evaluate, finally, try)
import Control.Monad (filterM, when)
import Data.List (sort)
import System.Directory
  ( copyFile
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , getTemporaryDirectory
  , listDirectory
  , removeFile
  , removePathForcibly
  )
import System.Exit (ExitCode(..))
import System.FilePath ((</>), dropExtension, takeBaseName, takeExtension, takeFileName)
import System.IO (BufferMode(..), hClose, hFlush, hSetBuffering, openTempFile, stdout)
import System.Process (readProcessWithExitCode)

import qualified GHC.IO.Handle as IOHandle

import qualified Pllisp.Driver as Driver

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
validTest path = it (takeBaseName path) $
  withCopiedFile ("valid-" ++ takeBaseName path) path $ \copiedPath -> do
    let expectedFile = dropExtension path ++ ".expected"
    hasExpected <- doesFileExist expectedFile
    (ec, out) <- runCompiler copiedPath
    case ec of
      ExitSuccess -> pure ()
      ExitFailure c -> expectationFailure ("expected compile success, got " ++ show c ++ ":\n" ++ out)
    result <- runCompiledBinary copiedPath
    when hasExpected $ do
      expected <- readFile expectedFile
      result `shouldBe` strip expected

invalidTest :: FilePath -> Spec
invalidTest path = it (takeBaseName path) $
  withCopiedFile ("invalid-" ++ takeBaseName path) path $ \copiedPath -> do
    let errorFile = dropExtension path ++ ".error"
    hasError <- doesFileExist errorFile
    (ec, out) <- runCompiler copiedPath
    ec `shouldBe` ExitFailure 1
    when hasError $ do
      expected <- readFile errorFile
      strip out `shouldContain` strip expected

strip :: String -> String
strip = reverse . dropWhile (== '\n') . reverse

findModuleExamples :: FilePath -> IO [FilePath]
findModuleExamples dir = do
  exists <- doesDirectoryExist dir
  if not exists then pure []
  else do
    entries <- listDirectory dir
    dirs <- filterM (\e -> doesDirectoryExist (dir </> e)) entries
    pure $ sort [dir </> d | d <- dirs]

validModuleTest :: FilePath -> Spec
validModuleTest dir = it (takeBaseName dir) $
  withCopiedDirectory ("valid-module-" ++ takeBaseName dir) dir $ \copiedDir -> do
    let mainPath = copiedDir </> "main.pllisp"
        expectedFile = dir </> "main.expected"
    hasExpected <- doesFileExist expectedFile
    (ec, out) <- runCompiler mainPath
    case ec of
      ExitSuccess -> pure ()
      ExitFailure c -> expectationFailure ("expected compile success, got " ++ show c ++ ":\n" ++ out)
    result <- runCompiledBinary mainPath
    when hasExpected $ do
      expected <- readFile expectedFile
      result `shouldBe` strip expected

invalidModuleTest :: FilePath -> Spec
invalidModuleTest dir = it (takeBaseName dir) $
  withCopiedDirectory ("invalid-module-" ++ takeBaseName dir) dir $ \copiedDir -> do
    let mainPath = copiedDir </> "main.pllisp"
        errorFile = dir </> "main.error"
    hasError <- doesFileExist errorFile
    (ec, out) <- runCompiler mainPath
    ec `shouldBe` ExitFailure 1
    when hasError $ do
      expected <- readFile errorFile
      strip out `shouldContain` strip expected

runCompiler :: FilePath -> IO (ExitCode, String)
runCompiler path = do
  (out, ec) <- captureStdout (Driver.runFiles [path])
  pure (ec, strip out)

runCompiledBinary :: FilePath -> IO String
runCompiledBinary path = do
  let exePath = dropExtension path
  (ec, out, err) <- readProcessWithExitCode exePath [] ""
  case ec of
    ExitSuccess   -> pure (strip out)
    ExitFailure c -> expectationFailure ("program exited with " ++ show c ++ ":\n" ++ err) >> pure ""

captureStdout :: IO a -> IO (String, a)
captureStdout action = do
  tmp <- getTemporaryDirectory
  bracket (openTempFile tmp "pllisp-example-spec.out") cleanup $ \(fp, h) -> do
    old <- IOHandle.hDuplicate stdout
    hSetBuffering stdout LineBuffering
    result <-
      (`finally` restoreStdout old h) $ do
        IOHandle.hDuplicateTo h stdout
        value <- action
        hFlush stdout
        pure value
    out <- readFile fp
    _ <- evaluate (length out)
    pure (out, result)
  where
    restoreStdout old h = do
      hFlush stdout
      IOHandle.hDuplicateTo old stdout
      ignoreIO (hClose old)
      ignoreIO (hClose h)

    cleanup (fp, h) = do
      ignoreIO (hClose h)
      ignoreIO (removeFile fp)

ignoreIO :: IO () -> IO ()
ignoreIO io = do
  _ <- try io :: IO (Either IOException ())
  pure ()

withCopiedFile :: String -> FilePath -> (FilePath -> IO a) -> IO a
withCopiedFile label src action =
  withScratchDir label $ \dir -> do
    let dst = dir </> takeFileName src
    copyFile src dst
    action dst

withCopiedDirectory :: String -> FilePath -> (FilePath -> IO a) -> IO a
withCopiedDirectory label srcDir action =
  withScratchDir label $ \dstDir -> do
    copyDirectoryContents srcDir dstDir
    action dstDir

withScratchDir :: String -> (FilePath -> IO a) -> IO a
withScratchDir label action = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "pllisp-example-spec" </> label
  clearIfExists dir
  createDirectoryIfMissing True dir
  action dir `finally` clearIfExists dir

clearIfExists :: FilePath -> IO ()
clearIfExists path = do
  dirExists <- doesDirectoryExist path
  when dirExists (removePathForcibly path)

copyDirectoryContents :: FilePath -> FilePath -> IO ()
copyDirectoryContents srcDir dstDir = do
  createDirectoryIfMissing True dstDir
  entries <- listDirectory srcDir
  mapM_ copyOne entries
  where
    copyOne name = do
      let src = srcDir </> name
          dst = dstDir </> name
      isDir <- doesDirectoryExist src
      if isDir
        then copyDirectoryContents src dst
        else do
          copyFile src dst
          when (takeExtension name == ".pllisp" && name /= "main.pllisp") $
            copyFile src (dstDir </> dropExtension name ++ ".pll")
