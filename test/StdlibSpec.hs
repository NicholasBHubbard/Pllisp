{-# LANGUAGE OverloadedStrings #-}

module StdlibSpec (spec) where

import Test.Hspec

import Control.Exception (IOException, bracket, evaluate, finally, try)
import Data.List (sort)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import System.Directory
  ( copyFile
  , createDirectoryIfMissing
  , getTemporaryDirectory
  , listDirectory
  , removeFile
  , removePathForcibly
  )
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeBaseName, takeExtension, takeFileName)
import System.IO (BufferMode(..), hClose, hFlush, hSetBuffering, openTempFile, stdout)

import qualified GHC.IO.Handle as IOHandle

import qualified Pllisp.Driver as Driver
import qualified Pllisp.Parser as Parser
import qualified Pllisp.SExpr as SExpr
import qualified Pllisp.SrcLoc as Loc

spec :: Spec
spec = do
  describe "PRELUDE style" $ do
    it "uses fun instead of raw let/lambda wrappers for named function definitions" $ do
      src <- T.IO.readFile "stdlib/PRELUDE.pll"
      sexprs <- case Parser.parseSExprs "stdlib/PRELUDE.pll" src of
        Left err -> expectationFailure ("parse error: " ++ show err) >> pure []
        Right parsed -> pure parsed
      collectWrappedNames sexprs `shouldBe` []

    it "does not expose the top-level splice sentinel in convenience macros" $ do
      src <- T.IO.readFile "stdlib/PRELUDE.pll"
      src `shouldNotContainText` "SPLICE-TOPLEVEL"

  describe "top-level splice usage" $ do
    it "is only needed by CLI among shipped stdlib modules" $ do
      files <- findStdlibFiles
      users <- findTopLevelSpliceUsers files
      users `shouldBe` ["stdlib/CLI.pll"]

  describe "stdlib modules" $ do
    files <- runIO findStdlibFiles
    mapM_ compileTest files

collectWrappedNames :: [SExpr.SExpr] -> [T.Text]
collectWrappedNames = concatMap go
  where
    go sx = case Loc.locVal sx of
      SExpr.SList (Loc.Located _ (SExpr.SAtom "EVAL-WHEN") : _phaseSx : bodyForms) ->
        collectWrappedNames bodyForms
      _ ->
        case wrappedName sx of
          Just name -> [name]
          Nothing -> []

wrappedName :: SExpr.SExpr -> Maybe T.Text
wrappedName (Loc.Located _
  (SExpr.SList
    [ Loc.Located _ (SExpr.SAtom "LET")
    , Loc.Located _ (SExpr.SList
        [ Loc.Located _ (SExpr.SList
            [ Loc.Located _ (SExpr.SAtom bindName)
            , Loc.Located _ (SExpr.SList (Loc.Located _ (SExpr.SAtom "LAM") : _))
            ])
        ])
    , Loc.Located _ (SExpr.SAtom bodyName)
    ]))
  | bindName == bodyName = Just bindName
wrappedName _ = Nothing

findStdlibFiles :: IO [FilePath]
findStdlibFiles = do
  files <- listDirectory "stdlib"
  pure $ sort ["stdlib" </> f | f <- files, takeExtension f == ".pll"]

compileTest :: FilePath -> Spec
compileTest path = it ("compiles " ++ takeBaseName path) $
  withScratchDir ("stdlib-" ++ takeBaseName path) $ \dir -> do
    let scratchStdlib = dir </> "stdlib"
        mainPath = dir </> "main.pllisp"
        modName = takeBaseName path
        mainSrc
          | modName == "PRELUDE" = "(print \"ok\")\n"
          | otherwise = "(import " ++ modName ++ ")\nunit\n"
    createDirectoryIfMissing True scratchStdlib
    stdlibFiles <- findStdlibFiles
    mapM_ (\src -> copyFile src (scratchStdlib </> takeFileName src)) stdlibFiles
    T.IO.writeFile mainPath (T.pack mainSrc)
    (out, ec) <- captureStdout (Driver.runFiles [mainPath])
    case ec of
      ExitSuccess -> pure ()
      ExitFailure c -> expectationFailure ("expected compile success, got " ++ show c ++ ":\n" ++ out)

captureStdout :: IO a -> IO (String, a)
captureStdout action = do
  tmp <- getTemporaryDirectory
  bracket (openTempFile tmp "pllisp-stdlib-spec.out") cleanup $ \(fp, h) -> do
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

withScratchDir :: String -> (FilePath -> IO a) -> IO a
withScratchDir label action = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "pllisp-stdlib-spec" </> label
  clearIfExists dir
  createDirectoryIfMissing True dir
  action dir `finally` clearIfExists dir

clearIfExists :: FilePath -> IO ()
clearIfExists dir = do
  _ <- try (removePathForcibly dir) :: IO (Either IOException ())
  pure ()

findTopLevelSpliceUsers :: [FilePath] -> IO [FilePath]
findTopLevelSpliceUsers = fmap sort . go
  where
    go [] = pure []
    go (path:rest) = do
      src <- T.IO.readFile path
      more <- go rest
      pure $
        if "SPLICE-TOPLEVEL" `T.isInfixOf` src
          then path : more
          else more

shouldNotContainText :: T.Text -> T.Text -> Expectation
shouldNotContainText haystack needle =
  T.isInfixOf needle haystack `shouldBe` False
