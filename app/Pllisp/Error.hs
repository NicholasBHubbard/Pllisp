-- MODULE

module Pllisp.Error where

import qualified Pllisp.SrcLoc as Loc

import qualified Data.Text as T

-- CORE

renderError :: T.Text -> String -> Loc.Span -> String -> String
renderError src kind sp msg =
  let start    = Loc.spanStart sp
      end      = Loc.spanEnd   sp
      lnum     = Loc.posLinum  start
      col      = Loc.posCol    start
      file     = Loc.posFile   start
      lnumStr  = show lnum
      lnumW    = length lnumStr
      pad      = replicate lnumW ' '
      srcLine  = case drop (lnum - 1) (T.lines src) of
                   (l:_) -> T.unpack l
                   []    -> ""
      caretLen = if Loc.posLinum end == lnum
                 then max 1 (Loc.posCol end - col)
                 else 1
      caret    = replicate (col - 1) ' ' ++ replicate caretLen '^'
  in unlines
       [ kind ++ " error: " ++ msg
       , pad ++ " --> " ++ file ++ ":" ++ lnumStr ++ ":" ++ show col
       , pad ++ " |"
       , lnumStr ++ " | " ++ srcLine
       , pad ++ " | " ++ caret
       , ""
       ]
