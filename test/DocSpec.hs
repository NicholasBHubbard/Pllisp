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

    it "documents SPLICE-TOPLEVEL as an internal top-level macro mechanism" $ do
      macrosDoc <- T.IO.readFile "doc/macros.md"
      mapM_ (macrosDoc `shouldContainText`)
        [ "SPLICE-TOPLEVEL"
        , "internal"
        , "multiple top-level forms"
        , "CLI"
        ]

      cliDoc <- T.IO.readFile "doc/stdlib/CLI.md"
      mapM_ (cliDoc `shouldContainText`)
        [ "SPLICE-TOPLEVEL"
        , "top-level bindings"
        ]

    it "documents the APPLICATIVE and MONAD stdlib modules" $ do
      readme <- T.IO.readFile "README.md"
      mapM_ (readme `shouldContainText`)
        [ "[APPLICATIVE](doc/stdlib/APPLICATIVE.md)"
        , "[MONAD](doc/stdlib/MONAD.md)"
        ]

      stdlibIndex <- T.IO.readFile "doc/stdlib/README.md"
      mapM_ (stdlibIndex `shouldContainText`)
        [ "[APPLICATIVE](APPLICATIVE.md)"
        , "[MONAD](MONAD.md)"
        ]

      applicativeDoc <- T.IO.readFile "doc/stdlib/APPLICATIVE.md"
      mapM_ (applicativeDoc `shouldContainText`)
        [ "# APPLICATIVE"
        , "(import APPLICATIVE"
        , "pure"
        , "ap"
        , "List"
        , "Maybe"
        , "Either"
        ]
      applicativeDoc `shouldNotContainText` "does not have a shipped `APPLICATIVE` instance yet."

      monadDoc <- T.IO.readFile "doc/stdlib/MONAD.md"
      mapM_ (monadDoc `shouldContainText`)
        [ "# MONAD"
        , "(import MONAD)"
        , "(import APPLICATIVE (pure))"
        , "do-let"
        , "implicit `progn`"
        , "does not implicitly call `pure`"
        , "List"
        , "Maybe"
        , "Either"
        ]
      monadDoc `shouldNotContainText` "does not have a shipped `MONAD` instance yet."

shouldContainText :: T.Text -> T.Text -> Expectation
shouldContainText haystack needle =
  T.isInfixOf needle haystack `shouldBe` True

shouldNotContainText :: T.Text -> T.Text -> Expectation
shouldNotContainText haystack needle =
  T.isInfixOf needle haystack `shouldBe` False
