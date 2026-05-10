{-# LANGUAGE OverloadedStrings #-}

module CliSpec (spec) where

import Test.Hspec

import Control.Exception (IOException, finally, try)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import System.Directory
  ( createDirectoryIfMissing
  , getTemporaryDirectory
  , removePathForcibly
  )
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO (Handle, IOMode(..), hClose, openFile)

import qualified Pllisp.Cli as Cli

spec :: Spec
spec = do
  describe "runCliWith" $ do
    it "runs an interactive repl session" $
      withCliIO "(print \"ok\")\n:quit\n" $ \inp out err outPath _errPath -> do
        ec <- Cli.runCliWith inp out err ["repl"]
        ec `shouldBe` ExitSuccess
        hClose out
        hClose err
        result <- T.IO.readFile outPath
        T.isInfixOf "ok" result `shouldBe` True

    it "preloads a file for repl" $
      withCliProject "(let ((x 7)) (print (int-to-str x)))\n" $ \fp inp out err outPath _errPath -> do
        ec <- Cli.runCliWith inp out err ["repl", fp]
        ec `shouldBe` ExitSuccess
        hClose out
        hClose err
        result <- T.IO.readFile outPath
        T.isInfixOf "7" result `shouldBe` True

    it "supports multiline input and repl commands" $
      withCliIO "(let ((x 41))\n  (print (int-to-str (add x 1))))\n:type (Just 1)\n:quit\n" $ \inp out err outPath _errPath -> do
        ec <- Cli.runCliWith inp out err ["repl"]
        ec `shouldBe` ExitSuccess
        hClose out
        hClose err
        result <- T.IO.readFile outPath
        T.isInfixOf "42" result `shouldBe` True
        T.isInfixOf "%(MAYBE %INT)" result `shouldBe` True

    it "returns failure for noninteractive repl errors" $
      withCliIO "(print missing)\n" $ \inp out err _outPath errPath -> do
        ec <- Cli.runCliWith inp out err ["repl"]
        ec `shouldBe` ExitFailure 1
        hClose out
        hClose err
        result <- T.IO.readFile errPath
        T.isInfixOf "symbol not in scope" result `shouldBe` True

withCliIO :: T.Text -> (Handle -> Handle -> Handle -> FilePath -> FilePath -> IO a) -> IO a
withCliIO input action = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "pllisp-cli-spec"
      inPath = dir </> "stdin.txt"
      outPath = dir </> "stdout.txt"
      errPath = dir </> "stderr.txt"
  createDirectoryIfMissing True dir
  T.IO.writeFile inPath input
  outInit <- openFile outPath WriteMode
  hClose outInit
  errInit <- openFile errPath WriteMode
  hClose errInit
  inH <- openFile inPath ReadMode
  outH <- openFile outPath AppendMode
  errH <- openFile errPath AppendMode
  action inH outH errH outPath errPath
    `finally` do
      ignoreIO (hClose inH)
      ignoreIO (hClose outH)
      ignoreIO (hClose errH)
      ignoreIO (removePathForcibly dir)

withCliProject :: T.Text -> (FilePath -> Handle -> Handle -> Handle -> FilePath -> FilePath -> IO a) -> IO a
withCliProject src action = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "pllisp-cli-project"
      fp = dir </> "script.pllisp"
  flip finally (ignoreIO (removePathForcibly dir)) $ do
    createDirectoryIfMissing True dir
    T.IO.writeFile fp src
    withCliIO ":quit\n" $ \inp out err outPath errPath -> action fp inp out err outPath errPath

ignoreIO :: IO () -> IO ()
ignoreIO io = do
  _ <- try io :: IO (Either IOException ())
  pure ()
