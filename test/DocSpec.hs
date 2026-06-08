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

    it "documents hierarchical module paths" $ do
      readme <- T.IO.readFile "README.md"
      mapM_ (readme `shouldContainText`)
        [ "hierarchical"
        , "`FOO.BAR.BAZ`"
        , "`FOO/BAR/BAZ.pll`"
        ]

      modulesDoc <- T.IO.readFile "doc/modules.md"
      mapM_ (modulesDoc `shouldContainText`)
        [ "hierarchical modules"
        , "(module FOO.BAR.BAZ)"
        , "(import FOO.BAR.BAZ)"
        , "`FOO/BAR/BAZ.pll`"
        ]

      replDoc <- T.IO.readFile "doc/repl.md"
      mapM_ (replDoc `shouldContainText`)
        [ "hierarchical module paths"
        , "`FOO.BAR.BAZ`"
        , "`FOO/BAR/BAZ.pll`"
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

    it "documents the FOLDABLE, APPLICATIVE, and MONAD stdlib modules" $ do
      readme <- T.IO.readFile "README.md"
      mapM_ (readme `shouldContainText`)
        [ "[FOLDABLE](doc/stdlib/FOLDABLE.md)"
        , "[APPLICATIVE](doc/stdlib/APPLICATIVE.md)"
        , "[MONAD](doc/stdlib/MONAD.md)"
        , "[TRAVERSABLE](doc/stdlib/TRAVERSABLE.md)"
        ]

      stdlibIndex <- T.IO.readFile "doc/stdlib/README.md"
      mapM_ (stdlibIndex `shouldContainText`)
        [ "[FOLDABLE](FOLDABLE.md)"
        , "[APPLICATIVE](APPLICATIVE.md)"
        , "[MONAD](MONAD.md)"
        , "[TRAVERSABLE](TRAVERSABLE.md)"
        ]

      foldableDoc <- T.IO.readFile "doc/stdlib/FOLDABLE.md"
      mapM_ (foldableDoc `shouldContainText`)
        [ "# FOLDABLE"
        , "(import FOLDABLE"
        , "foldr"
        , "foldl"
        , "List"
        , "Maybe"
        , "Either"
        , "Pair"
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

      traversableDoc <- T.IO.readFile "doc/stdlib/TRAVERSABLE.md"
      mapM_ (traversableDoc `shouldContainText`)
        [ "# TRAVERSABLE"
        , "(import TRAVERSABLE"
        , "traverse"
        , "APPLICATIVE"
        , "List"
        , "Maybe"
        , "Either"
        , "Pair"
        ]

shouldContainText :: T.Text -> T.Text -> Expectation
shouldContainText haystack needle =
  T.isInfixOf needle haystack `shouldBe` True

shouldNotContainText :: T.Text -> T.Text -> Expectation
shouldNotContainText haystack needle =
  T.isInfixOf needle haystack `shouldBe` False
