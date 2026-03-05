module Main (main) where

import qualified Data.Text.IO as T.IO
import qualified Pllisp.Parser as Parser
import qualified Pllisp.Resolve as Resolve
import qualified Pllisp.TypeCheck as TC

main :: IO ()
main = do
  src <- T.IO.getContents
  case Parser.parseProgram src of
    Left err -> putStrLn $ "parse error: " ++ show err
    Right cst ->
      case Resolve.resolve cst of
        Left errs -> putStrLn $ "resolve error: " ++ show errs
        Right resolved ->
          case TC.typecheck resolved of
            Left err -> putStrLn $ "type error: " ++ TC.teMsg err
            Right typed -> print typed
