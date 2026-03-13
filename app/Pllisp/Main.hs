module Main (main) where

import System.Environment (getArgs)

import qualified Data.Text.IO as T.IO
import qualified Text.Megaparsec as MP

import qualified Pllisp.Error     as Error
import qualified Pllisp.Exhaust   as Exhaust
import qualified Pllisp.Parser    as Parser
import qualified Pllisp.Resolve   as Resolve
import qualified Pllisp.TypeCheck as TC

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
  case Parser.parseProgram fp src of
    Left  err -> putStr (MP.errorBundlePretty err)
    Right cst ->
      case Resolve.resolve cst of
        Left errs -> mapM_ (\e -> render "resolve" (Resolve.errSpan e) (Resolve.errMsg e)) errs
        Right resolved ->
          case TC.typecheck resolved of
            Left errs -> mapM_ (\e -> render "type" (TC.teSpan e) (TC.teMsg e)) errs
            Right typed ->
              case Exhaust.exhaustCheck typed of
                [] -> print typed
                errs -> mapM_ (\e -> render "exhaust" (Exhaust.exhaSpan e) (Exhaust.exhaMsg e)) errs
