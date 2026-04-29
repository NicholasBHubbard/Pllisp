{-# LANGUAGE OverloadedStrings #-}

module EditorSpec (spec) where

import Test.Hspec

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

spec :: Spec
spec = do
  describe "pllisp-mode keyword surface" $ do
    it "matches the current core forms and implicit PRELUDE macros" $ do
      specials <- extractConstStrings "pllisp-special-forms"
      preludeMacros <- extractConstStrings "pllisp-prelude-macros"
      let actual = S.fromList (specials ++ preludeMacros)
      actual `shouldBe` expectedKeywordNames

    it "includes eval-when and macro convenience forms as keywords" $ do
      specials <- extractConstStrings "pllisp-special-forms"
      preludeMacros <- extractConstStrings "pllisp-prelude-macros"
      let actual = S.fromList (specials ++ preludeMacros)
      mapM_ (\name -> S.member name actual `shouldBe` True)
        [ "eval-when", "syntax-case", "fun", "progn", "if_", "when", "unless"
        , "cond", "if-let", "when-let", "unless-let", "and", "or"
        ]

  describe "pllisp-mode builtin surface" $ do
    it "matches the current implicit runtime and compile-time function names" $ do
      builtins <- S.fromList <$> extractConstStrings "pllisp-builtins"
      builtins `shouldBe` expectedBuiltinNames

    it "does not keep removed or reclassified names in the builtin list" $ do
      builtins <- S.fromList <$> extractConstStrings "pllisp-builtins"
      mapM_ (\name -> S.member name builtins `shouldBe` False)
        [ "gc-collect", "gc-heap-size"
        , "fun", "progn", "if_", "when", "unless"
        , "cond", "if-let", "when-let", "unless-let", "and", "or"
        , "car", "cdr", "cons", "list", "length"
        , "null?", "symbol?", "list?", "string?", "number?"
        , "bool?", "type?"
        , "sym-to-str", "str-to-sym"
        ]

extractConstStrings :: T.Text -> IO [T.Text]
extractConstStrings name = do
  src <- T.IO.readFile "extras/pllisp-mode.el"
  let defMarker = "(defconst " <> name
      (_, afterDef) = T.breakOn defMarker src
  if T.null afterDef
    then expectationFailure ("missing defconst " ++ T.unpack name) >> pure []
    else do
      let (_, afterListMarker) = T.breakOn "'(" afterDef
      if T.null afterListMarker
        then expectationFailure ("missing quoted list for " ++ T.unpack name) >> pure []
        else pure (parseQuotedStrings (T.drop 2 afterListMarker))

parseQuotedStrings :: T.Text -> [T.Text]
parseQuotedStrings txt =
  case T.uncons txt of
    Nothing -> []
    Just (')', _) -> []
    Just ('"', rest) ->
      let (str, after) = T.breakOn "\"" rest
      in str : parseQuotedStrings (T.drop 1 after)
    Just (_, rest) -> parseQuotedStrings rest

expectedKeywordNames :: S.Set T.Text
expectedKeywordNames = S.fromList
  [ "lam", "let", "if", "case", "type", "cls", "inst"
  , "module", "import", "mac", "eval-when", "syntax-case", "ffi", "ffi-struct"
  , "ffi-var", "ffi-enum", "ffi-callback"
  , "fun", "progn", "if_", "when", "unless", "cond"
  , "if-let", "when-let", "unless-let", "and", "or"
  ]

expectedBuiltinNames :: S.Set T.Text
expectedBuiltinNames = S.fromList
  [ "add", "sub", "mul", "div", "mod"
  , "addf", "subf", "mulf", "divf"
  , "eqi", "lti"
  , "eqf", "ltf"
  , "eqs", "lts"
  , "concat", "strlen", "substr"
  , "int-to-flt", "flt-to-int"
  , "usym-to-str", "str-to-usym"
  , "ref", "deref", "set!"
  , "not", "neg", "negf"
  , "gti", "lei", "gei"
  , "gtf", "lef", "gef"
  , "gts", "les", "ges"
  , "str-contains"
  , "print", "read-line", "is-eof"
  , "argc", "argv"
  , "int-to-str", "flt-to-str"
  , "rx-match", "rx-find", "rx-sub", "rx-gsub"
  , "rx-split", "rx-captures", "rx-compile"
  , "truthy", "eq", "lt", "gt", "le", "ge", "str"
  , "append", "reverse", "map", "filter", "foldl"
  , "syntax-lift"
  , "syntax-symbol", "syntax-int", "syntax-float", "syntax-string"
  , "syntax-bool", "syntax-usym", "syntax-rx", "syntax-type"
  , "syntax-cons", "syntax-append"
  , "syntax-car", "syntax-cdr", "syntax-length", "syntax-equal?"
  , "syntax-null?", "syntax-symbol?", "syntax-list?"
  , "syntax-string?", "syntax-number?", "syntax-bool?"
  , "syntax-type?"
  , "syntax-int-value", "syntax-float-value", "syntax-string-value"
  , "syntax-symbol-name", "syntax-usym-name"
  , "error"
  ]
