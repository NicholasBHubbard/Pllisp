module Pllisp.Stdlib (loadPrelude, getStdlibDir, getStdlibDirNear) where

import qualified Pllisp.SExpr  as SExpr
import qualified Pllisp.Parser as Parser

import qualified Data.Text.IO as T.IO
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath (takeDirectory, (</>))
import Data.List (nub)
import Paths_pllisp (getDataFileName)

loadPrelude :: IO [SExpr.SExpr]
loadPrelude = do
  fp <- getPreludeFile []
  src <- T.IO.readFile fp
  case Parser.parseSExprs "<prelude>" src of
    Left _    -> error "BUG: prelude failed to parse"
    Right sexprs -> pure sexprs

getStdlibDir :: IO FilePath
getStdlibDir = getStdlibDirNear []

getStdlibDirNear :: [FilePath] -> IO FilePath
getStdlibDirNear hints = takeDirectory <$> getPreludeFile hints

getPreludeFile :: [FilePath] -> IO FilePath
getPreludeFile hints = do
  primary <- getDataFileName "PRELUDE.pll"
  cwd <- getCurrentDirectory
  let candidates =
        nub $
          primary :
          concatMap candidatePaths (concatMap ancestors (hints ++ [cwd]))
  firstExisting candidates primary

candidatePaths :: FilePath -> [FilePath]
candidatePaths dir =
  [ dir </> "PRELUDE.pll"
  , dir </> "stdlib" </> "PRELUDE.pll"
  ]

ancestors :: FilePath -> [FilePath]
ancestors dir
  | parent == dir = [dir]
  | otherwise = dir : ancestors parent
  where
    parent = takeDirectory dir

firstExisting :: [FilePath] -> FilePath -> IO FilePath
firstExisting [] fallback = pure fallback
firstExisting (fp:fps) fallback = do
  exists <- doesFileExist fp
  if exists
    then pure fp
    else firstExisting fps fallback
