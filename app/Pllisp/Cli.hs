{-# LANGUAGE OverloadedStrings #-}

module Pllisp.Cli
  ( runCli
  , runCliWith
  ) where

import System.Exit (ExitCode(..))
import System.IO
  ( Handle
  , hFlush
  , hIsEOF
  , hIsTerminalDevice
  , stderr
  , stdin
  , stdout
  )

import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

import qualified Pllisp.Driver as Driver
import qualified Pllisp.Repl as Repl

runCli :: [String] -> IO ExitCode
runCli = runCliWith stdin stdout stderr

runCliWith :: Handle -> Handle -> Handle -> [String] -> IO ExitCode
runCliWith inh outh errh args =
  case args of
    [] -> T.IO.hPutStrLn outh usage >> pure ExitSuccess
    ["repl"] -> runReplLoop inh outh errh Nothing
    ["repl", fp] -> runReplLoop inh outh errh (Just fp)
    ("repl":_) -> T.IO.hPutStrLn errh "usage: pllisp repl [file]" >> pure (ExitFailure 1)
    _ -> Driver.runFiles args

usage :: T.Text
usage = T.unlines
  [ "usage: pllisp <file> [<file>...]"
  , "       pllisp repl [file]"
  ]

runReplLoop :: Handle -> Handle -> Handle -> Maybe FilePath -> IO ExitCode
runReplLoop inh outh errh preload = do
  interactive <- hIsTerminalDevice inh
  sess <- Repl.newSession Repl.defaultConfig
  let cleanup = Repl.closeSession sess
  case preload of
    Nothing -> loop interactive sess []
    Just fp -> do
      preloadResult <- Repl.loadFile sess fp
      case preloadResult of
        Left err -> printErr err >> cleanup >> pure (ExitFailure 1)
        Right ok -> do
          T.IO.hPutStr outh (Repl.reStdout ok)
          loop interactive sess []
  where
    loop interactive sess chunks = do
      if interactive
        then T.IO.hPutStr outh (if null chunks then "pllisp> " else "......> ")
        else pure ()
      hFlush outh
      eof <- hIsEOF inh
      if eof
        then finishEOF sess chunks
        else do
          line <- T.IO.hGetLine inh
          let stripped = T.strip line
          if null chunks && ":" `T.isPrefixOf` stripped
            then do
              commandResult <- handleCommand interactive sess stripped
              case commandResult of
                Left ec -> Repl.closeSession sess >> pure ec
                Right () -> loop interactive sess []
            else do
              let nextChunks = chunks ++ [line]
                  src = T.unlines nextChunks
              if T.null (T.strip src)
                then loop interactive sess []
                else if isIncomplete src
                  then loop interactive sess nextChunks
                  else do
                    sourceResult <- runSource interactive sess src
                    case sourceResult of
                      Left ec -> Repl.closeSession sess >> pure ec
                      Right () -> loop interactive sess []

    finishEOF sess [] = Repl.closeSession sess >> pure ExitSuccess
    finishEOF sess chunks =
      let src = T.unlines chunks
      in if isIncomplete src
           then do
             T.IO.hPutStrLn errh "unexpected EOF while reading repl input"
             Repl.closeSession sess
             pure (ExitFailure 1)
           else do
             sourceResult <- runSource False sess src
             Repl.closeSession sess
             pure $
               case sourceResult of
                 Left ec -> ec
                 Right () -> ExitSuccess

    runSource interactive sess src = do
      result <- Repl.submitForms sess src
      case result of
        Left err -> do
          printErr err
          pure (if interactive then Right () else Left (ExitFailure 1))
        Right ok -> T.IO.hPutStr outh (Repl.reStdout ok) >> pure (Right ())

    handleCommand interactive sess line = case T.words line of
      [":quit"] -> pure (Left ExitSuccess)
      [":help"] -> helpText >> pure (Right ())
      [":reset"] -> Repl.resetSession sess >> pure (Right ())
      [":reload"] -> do
        result <- Repl.reloadSession sess
        case result of
          Left err -> printErr err >> failOrContinue interactive
          Right ok -> T.IO.hPutStr outh (Repl.reStdout ok) >> pure (Right ())
      (":load":rest) -> do
        if null rest
          then T.IO.hPutStrLn errh "usage: :load <file>" >> failOrContinue interactive
          else do
            result <- Repl.loadFile sess (T.unpack (T.unwords rest))
            case result of
              Left err -> printErr err >> failOrContinue interactive
              Right ok -> T.IO.hPutStr outh (Repl.reStdout ok) >> pure (Right ())
      (":type":rest) -> do
        if null rest
          then T.IO.hPutStrLn errh "usage: :type <expr>" >> failOrContinue interactive
          else do
            result <- Repl.typeOf sess (T.unwords rest)
            case result of
              Left err -> printErr err >> failOrContinue interactive
              Right info -> T.IO.hPutStrLn outh (Repl.rtiRendered info) >> pure (Right ())
      (":macroexpand":rest) -> do
        if null rest
          then T.IO.hPutStrLn errh "usage: :macroexpand <form>" >> failOrContinue interactive
          else do
            result <- Repl.macroExpand sess (T.unwords rest)
            case result of
              Left err -> printErr err >> failOrContinue interactive
              Right info -> T.IO.hPutStrLn outh (Repl.rmiRendered info) >> pure (Right ())
      _ -> T.IO.hPutStrLn errh "unknown repl command" >> failOrContinue interactive

    helpText =
      T.IO.hPutStrLn outh $
        T.unlines
          [ ":quit"
          , ":help"
          , ":load <file>"
          , ":reload"
          , ":reset"
          , ":type <expr>"
          , ":macroexpand <form>"
          ]

    printErr err = T.IO.hPutStrLn errh (Repl.reMessage err)

    failOrContinue interactive =
      pure (if interactive then Right () else Left (ExitFailure 1))

isIncomplete :: T.Text -> Bool
isIncomplete = go (0 :: Int) False False False . T.unpack
  where
    go depth inString escaped _inComment [] =
      inString || escaped || depth > 0
    go depth inString escaped inComment (c:cs)
      | inComment =
          if c == '\n'
            then go depth inString escaped False cs
            else go depth inString escaped True cs
      | inString =
          if escaped
            then go depth True False False cs
            else case c of
              '\\' -> go depth True True False cs
              '"' -> go depth False False False cs
              _ -> go depth True False False cs
      | otherwise =
          case c of
            '#' -> go depth False False True cs
            '"' -> go depth True False False cs
            '(' -> go (depth + 1) False False False cs
            ')' -> go (max 0 (depth - 1)) False False False cs
            _ -> go depth False False False cs
