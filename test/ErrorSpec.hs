{-# LANGUAGE OverloadedStrings #-}

module ErrorSpec (spec) where

import Test.Hspec
import Data.List (isPrefixOf, isInfixOf)

import qualified Data.Text     as T
import qualified Pllisp.Error  as Error
import qualified Pllisp.SrcLoc as Loc

spec :: Spec
spec = do
  describe "renderError" $ do
    let src = "hello world\nsecond line\nthird line" :: T.Text

    it "contains the kind and message" $ do
      let out = renderE src "type" (sp 1 1 1 6) "bad thing"
      "type error: bad thing" `isPrefixOf` out `shouldBe` True

    it "contains the file, line, col in --> line" $ do
      let out = renderE src "resolve" (sp 1 1 1 1) "oops"
      " --> <test>:1:1" `isInfixOf` out `shouldBe` True

    it "single-line span col 1: caret at column 1" $ do
      let out = renderE src "type" (sp 1 1 1 2) "err"
          ls  = lines out
      -- pad=" " (1 space for lnum=1), so "  | ^"
      ls !! 4 `shouldBe` "  | ^"

    it "single-line span col 4: 3 spaces then caret" $ do
      let out = renderE src "type" (sp 1 4 1 5) "err"
          ls  = lines out
      ls !! 4 `shouldBe` "  |    ^"

    it "single-line span multi-col: 5 carets" $ do
      let out = renderE src "type" (sp 1 1 1 6) "err"
          ls  = lines out
      -- caretLen = max 1 (6 - 1) = 5
      ls !! 4 `shouldBe` "  | ^^^^^"

    it "multi-line span: single caret" $ do
      let out = renderE src "type" (sp 1 1 2 3) "err"
          ls  = lines out
      ls !! 4 `shouldBe` "  | ^"

    it "source line is included in output" $ do
      let out = renderE src "type" (sp 2 1 2 1) "err"
      "second line" `isInfixOf` out `shouldBe` True

    it "line out of range: srcLine is empty" $ do
      let out = renderE src "type" (sp 99 1 99 1) "err"
      "99 | \n" `isInfixOf` out `shouldBe` True

    it "lnum 1: first line is correct (no off-by-one)" $ do
      let out = renderE src "type" (sp 1 1 1 1) "err"
      "hello world" `isInfixOf` out `shouldBe` True

    it "output has 6 lines (including trailing blank)" $ do
      let out = renderE src "type" (sp 1 1 1 1) "err"
      length (lines out) `shouldBe` 6

    it "single-digit lnum: 1-char pad on --> line" $ do
      let out = renderE src "type" (sp 1 1 1 1) "err"
      -- lnumW=1, pad=" ", so " " ++ " --> " = "  --> "
      (lines out !! 1) `shouldBe` "  --> <test>:1:1"

    it "double-digit lnum: 2-char pad on --> line" $ do
      let bigSrc = T.unlines (replicate 12 "x")
          out = renderE bigSrc "type" (sp 11 1 11 1) "err"
      (lines out !! 1) `shouldBe` "   --> <test>:11:1"

-- Helpers

sp :: Int -> Int -> Int -> Int -> Loc.Span
sp l1 c1 l2 c2 =
  Loc.Span (Loc.Pos "<test>" l1 c1) (Loc.Pos "<test>" l2 c2)

renderE :: T.Text -> String -> Loc.Span -> String -> String
renderE = Error.renderError
