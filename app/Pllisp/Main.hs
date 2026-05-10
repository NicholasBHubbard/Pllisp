module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitWith)

import qualified Pllisp.Cli as Cli

main :: IO ()
main = do
  args <- getArgs
  Cli.runCli args >>= exitWith
