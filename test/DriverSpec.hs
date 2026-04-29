{-# LANGUAGE OverloadedStrings #-}

module DriverSpec (spec) where

import Test.Hspec

import Control.Exception (bracket, finally)
import qualified Data.Text as T
import System.Directory
  ( createDirectoryIfMissing
  , getCurrentDirectory
  , getTemporaryDirectory
  , doesFileExist
  , copyFile
  , removePathForcibly
  , setCurrentDirectory
  )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeDirectory, dropExtension, takeFileName)
import qualified Data.Text.IO as T.IO

import qualified Pllisp.Driver as Driver

spec :: Spec
spec = do
  describe "runFiles" $ do
    it "returns failure for semantic errors" $
      withTempSource "semantic-error.pll" "(add x 1)" $ \fp ->
        Driver.runFiles [fp] `shouldReturn` ExitFailure 1

    it "falls back to the workspace stdlib when cabal data lookup is missing" $
      withRepoRoot $ do
        withEnvVar "pllisp_datadir" "/definitely/missing" $
          withTempSource "fallback-stdlib.pll" "(print \"ok\")" $ \fp ->
            Driver.runFiles [fp] `shouldReturn` ExitSuccess

    it "rejects malformed module declarations" $
      withTempSource "bad-module.pll" "(module)" $ \fp ->
        Driver.runFiles [fp] `shouldReturn` ExitFailure 1

    it "rejects malformed import declarations" $
      withTempSource "bad-import.pll" "(import)" $ \fp ->
        Driver.runFiles [fp] `shouldReturn` ExitFailure 1

    it "rejects malformed macro definitions" $
      withTempSource "bad-macro.pll" "(mac broken)" $ \fp ->
        Driver.runFiles [fp] `shouldReturn` ExitFailure 1

    it "rejects reserved words as binding names" $
      withTempSource "reserved-binding.pll" "(let ((if 1)) if)" $ \fp ->
        Driver.runFiles [fp] `shouldReturn` ExitFailure 1

    it "rejects top-level redefinitions of PRELUDE symbols" $
      withTempSource "redefine-prelude.pll" "(fun just ((x %INT)) x)" $ \fp ->
        Driver.runFiles [fp] `shouldReturn` ExitFailure 1

    it "rejects top-level redefinitions of PRELUDE macro names" $
      withTempSource "redefine-prelude-macro.pll" "(let ((fun 1)) fun)" $ \fp ->
        Driver.runFiles [fp] `shouldReturn` ExitFailure 1

    it "rejects duplicate imported compile-time helpers" $
      withTempModuleProject
        [ ("A.pll", unlines
            [ "(module A)"
            , "(eval-when (:compile-toplevel)"
            , "  (let ((emit-double (lam (x) `(add ,x ,x))))"
            , "    emit-double))"
            ])
        , ("B.pll", unlines
            [ "(module B)"
            , "(eval-when (:compile-toplevel)"
            , "  (let ((emit-double (lam (x) `(mul ,x 2))))"
            , "    emit-double))"
            ])
        ]
        (unlines
          [ "(import A)"
          , "(import B)"
          , "unit"
          ])
        $ \fp -> Driver.runFiles [fp] `shouldReturn` ExitFailure 1

    it "rejects duplicate imported macros" $
      withTempModuleProject
        [ ("A.pll", unlines
            [ "(module A)"
            , "(mac double (x) `(add ,x ,x))"
            ])
        , ("B.pll", unlines
            [ "(module B)"
            , "(mac double (x) `(mul ,x 2))"
            ])
        ]
        (unlines
          [ "(import A)"
          , "(import B)"
          , "(print (int-to-str (double 21)))"
          ])
        $ \fp -> Driver.runFiles [fp] `shouldReturn` ExitFailure 1

withRepoRoot :: IO a -> IO a
withRepoRoot action = do
  cwd <- getCurrentDirectory
  repoRoot <- findRepoRoot cwd
  bracket (getCurrentDirectory <* setCurrentDirectory repoRoot)
          setCurrentDirectory
          (const action)

findRepoRoot :: FilePath -> IO FilePath
findRepoRoot dir = do
  let preludeFile = dir </> "stdlib" </> "PRELUDE.pll"
  exists <- doesFileExist preludeFile
  if exists
    then pure dir
    else do
      let parent = takeDirectory dir
      if parent == dir
        then expectationFailure "could not find repo root" >> pure dir
        else findRepoRoot parent

withEnvVar :: String -> String -> IO a -> IO a
withEnvVar name value action = do
  old <- lookupEnv name
  setEnv name value
  action `finally` restore old
  where
    restore (Just old) = setEnv name old
    restore Nothing = unsetEnv name

withTempSource :: FilePath -> String -> (FilePath -> IO a) -> IO a
withTempSource name src action = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "pllisp-driver-spec"
  createDirectoryIfMissing True dir
  bracket (makeRunDir dir) removePathForcibly $ \runDir -> do
    let fp = runDir </> name
    T.IO.writeFile fp (T.pack src)
    action fp
  where
    makeRunDir base = do
      let runDir = base </> dropExtension name
      createDirectoryIfMissing True runDir
      pure runDir

withTempModuleProject :: [(FilePath, String)] -> String -> (FilePath -> IO a) -> IO a
withTempModuleProject files mainSrc action = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "pllisp-driver-spec"
  createDirectoryIfMissing True dir
  bracket (makeRunDir dir) removePathForcibly $ \runDir -> do
    preloadStdlib runDir
    mapM_ (\(name, src) -> T.IO.writeFile (runDir </> name) (T.pack src)) files
    let mainPath = runDir </> "main.pllisp"
    T.IO.writeFile mainPath (T.pack mainSrc)
    action mainPath
  where
    makeRunDir base = do
      let runDir = base </> "modules"
      createDirectoryIfMissing True runDir
      pure runDir

preloadStdlib :: FilePath -> IO ()
preloadStdlib runDir = do
  cwd <- getCurrentDirectory
  repoRoot <- findRepoRoot cwd
  let preludePath = repoRoot </> "stdlib" </> "PRELUDE.pll"
  copyFile preludePath (runDir </> takeFileName preludePath)
