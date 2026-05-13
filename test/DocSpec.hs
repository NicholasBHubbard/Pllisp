{-# LANGUAGE OverloadedStrings #-}

module DocSpec (spec) where

import Test.Hspec

import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

spec :: Spec
spec = do
  describe "manual" $ do
    it "links the repl guide from the top-level manual index" $ do
      readme <- T.IO.readFile "README.md"
      readme `shouldContainText` "[REPL](doc/repl.md)"

    it "documents the repl commands and usage" $ do
      replDoc <- T.IO.readFile "doc/repl.md"
      mapM_ (replDoc `shouldContainText`)
        [ "# REPL"
        , "pllisp repl"
        , "pllisp repl path/to/file.pllisp"
        , ":quit"
        , ":help"
        , ":load FILE"
        , ":reload"
        , ":reset"
        , ":type EXPR"
        , ":macroexpand FORM"
        , "The REPL does not automatically print arbitrary values."
        ]

shouldContainText :: T.Text -> T.Text -> Expectation
shouldContainText haystack needle =
  T.isInfixOf needle haystack `shouldBe` True
