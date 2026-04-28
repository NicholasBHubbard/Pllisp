module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitWith)

import qualified Pllisp.Driver as Driver

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "usage: pllisp <file> [<file>...]"
    _  -> Driver.runFiles args >>= exitWith
