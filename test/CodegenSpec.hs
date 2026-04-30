{-# LANGUAGE OverloadedStrings #-}

module CodegenSpec (spec) where

import Test.Hspec

import Control.Monad (foldM)
import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import qualified Data.Text       as T
import qualified Data.Text.IO as T.IO
import Control.Exception (ErrorCall(..), try, evaluate)
import Data.List (isInfixOf)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

import qualified Pllisp.Codegen        as Codegen
import qualified Pllisp.ClosureConvert as CC
import qualified Pllisp.Type           as Ty
import qualified Pllisp.CST            as CST
import qualified Pllisp.LambdaLift     as LL
import qualified Pllisp.MacroExpand    as MacroExpand
import qualified Pllisp.Module         as Mod
import qualified Pllisp.Parser         as Parser
import qualified Pllisp.SExpr          as SExpr
import qualified Pllisp.Stdlib         as Stdlib
import qualified Pllisp.Resolve        as Resolve
import qualified Pllisp.TypeCheck      as TC

spec :: Spec
spec = do
  describe "integer arithmetic" $ do
    it "addition"       $ run "(print (int-to-str (add 1 2)))"   >>= (`shouldBe` "3")
    it "subtraction"    $ run "(print (int-to-str (sub 10 3)))"  >>= (`shouldBe` "7")
    it "multiplication" $ run "(print (int-to-str (mul 6 7)))"   >>= (`shouldBe` "42")
    it "division"       $ run "(print (int-to-str (div 10 3)))"  >>= (`shouldBe` "3")
    it "modulo"         $ run "(print (int-to-str (mod 10 3)))"  >>= (`shouldBe` "1")
    it "negation"       $ run "(print (int-to-str (neg 42)))"    >>= (`shouldBe` "-42")

  describe "float arithmetic" $ do
    it "addition"       $ run "(print (flt-to-str (addf 1.5 2.5)))" >>= (`shouldBe` "4")
    it "subtraction"    $ run "(print (flt-to-str (subf 5.0 1.5)))" >>= (`shouldBe` "3.5")
    it "multiplication" $ run "(print (flt-to-str (mulf 2.0 3.5)))" >>= (`shouldBe` "7")
    it "division"       $ run "(print (flt-to-str (divf 7.0 2.0)))" >>= (`shouldBe` "3.5")
    it "negation"       $ run "(print (flt-to-str (negf 3.14)))"    >>= (`shouldBe` "-3.14")

  describe "integer comparisons" $ do
    it "eqi true"  $ run "(print (int-to-str (if (eqi 1 1) 1 0)))" >>= (`shouldBe` "1")
    it "eqi false" $ run "(print (int-to-str (if (eqi 1 2) 1 0)))" >>= (`shouldBe` "0")
    it "lti"      $ run "(print (int-to-str (if (lti 1 2) 1 0)))" >>= (`shouldBe` "1")
    it "gti"      $ run "(print (int-to-str (if (gti 2 1) 1 0)))" >>= (`shouldBe` "1")
    it "lei"      $ run "(print (int-to-str (if (lei 1 1) 1 0)))" >>= (`shouldBe` "1")
    it "gei"      $ run "(print (int-to-str (if (gei 2 1) 1 0)))" >>= (`shouldBe` "1")

  describe "float comparisons" $ do
    it "eqf" $ run "(print (int-to-str (if (eqf 1.0 1.0) 1 0)))" >>= (`shouldBe` "1")
    it "ltf" $ run "(print (int-to-str (if (ltf 1.0 2.0) 1 0)))" >>= (`shouldBe` "1")

  describe "boolean operations" $ do
    it "and tt" $ run "(print (int-to-str (if (and true true)   1 0)))" >>= (`shouldBe` "1")
    it "and tf" $ run "(print (int-to-str (if (and true false)  1 0)))" >>= (`shouldBe` "0")
    it "or tf"  $ run "(print (int-to-str (if (or true false)   1 0)))" >>= (`shouldBe` "1")
    it "or ff"  $ run "(print (int-to-str (if (or false false)  1 0)))" >>= (`shouldBe` "0")
    it "not"    $ run "(print (int-to-str (if (not false) 1 0)))"       >>= (`shouldBe` "1")
    it "and short-circuits" $
      run (T.unlines
        [ "(let ((r (ref 0)))"
        , "  (let ((_ (and false (let ((_ (set! r 1))) true))))"
        , "    (print (int-to-str (deref r)))))"
        ]) >>= (`shouldBe` "0")
    it "or short-circuits" $
      run (T.unlines
        [ "(let ((r (ref 0)))"
        , "  (let ((_ (or true (let ((_ (set! r 1))) false))))"
        , "    (print (int-to-str (deref r)))))"
        ]) >>= (`shouldBe` "0")

  describe "strings" $ do
    it "prints string literal"  $ run "(print \"hello\")"                      >>= (`shouldBe` "hello")
    it "concatenation"          $ run "(print (concat \"hello\" \" world\"))"  >>= (`shouldBe` "hello world")
    it "string length"          $ run "(print (int-to-str (strlen \"hello\")))" >>= (`shouldBe` "5")
    it "string equality true"   $ run "(print (int-to-str (if (eqs \"a\" \"a\") 1 0)))" >>= (`shouldBe` "1")
    it "string equality false"  $ run "(print (int-to-str (if (eqs \"a\" \"b\") 1 0)))" >>= (`shouldBe` "0")
    it "lts true"  $ run "(print (int-to-str (if (lts \"a\" \"b\") 1 0)))" >>= (`shouldBe` "1")
    it "lts false" $ run "(print (int-to-str (if (lts \"b\" \"a\") 1 0)))" >>= (`shouldBe` "0")
    it "gts true"  $ run "(print (int-to-str (if (gts \"b\" \"a\") 1 0)))" >>= (`shouldBe` "1")
    it "gts false" $ run "(print (int-to-str (if (gts \"a\" \"b\") 1 0)))" >>= (`shouldBe` "0")
    it "les true"  $ run "(print (int-to-str (if (les \"a\" \"a\") 1 0)))" >>= (`shouldBe` "1")
    it "les false" $ run "(print (int-to-str (if (les \"b\" \"a\") 1 0)))" >>= (`shouldBe` "0")
    it "ges true"  $ run "(print (int-to-str (if (ges \"b\" \"a\") 1 0)))" >>= (`shouldBe` "1")
    it "ges false" $ run "(print (int-to-str (if (ges \"a\" \"b\") 1 0)))" >>= (`shouldBe` "0")

  describe "conversions" $ do
    it "int to float"  $ run "(print (flt-to-str (int-to-flt 42)))" >>= (`shouldBe` "42")
    it "float to int"  $ run "(print (int-to-str (flt-to-int 3.7)))" >>= (`shouldBe` "3")
    it "int to string" $ run "(print (int-to-str 99))"              >>= (`shouldBe` "99")

  describe "let bindings" $ do
    it "simple let" $
      run "(let ((x 42)) (print (int-to-str x)))" >>= (`shouldBe` "42")
    it "multiple bindings" $
      run "(let ((x 1) (y 2)) (print (int-to-str (add x y))))" >>= (`shouldBe` "3")
    it "sequential binding" $
      run "(let ((x 10) (y (add x 5))) (print (int-to-str y)))" >>= (`shouldBe` "15")
    it "nested let" $
      run "(let ((x (let ((y 10)) (add y 1)))) (print (int-to-str x)))" >>= (`shouldBe` "11")

  describe "if expressions" $ do
    it "true branch"  $ run "(print (int-to-str (if true 1 2)))"  >>= (`shouldBe` "1")
    it "false branch" $ run "(print (int-to-str (if false 1 2)))" >>= (`shouldBe` "2")
    it "nested if" $
      run "(print (int-to-str (if true (if false 1 2) 3)))" >>= (`shouldBe` "2")

  describe "closures" $ do
    it "simple lambda" $
      run "(let ((f (lam ((x %INT)) (add x 1)))) (print (int-to-str (f 41))))"
        >>= (`shouldBe` "42")
    it "closure captures variable" $
      run "(let ((y 10)) (let ((f (lam ((x %INT)) (add x y)))) (print (int-to-str (f 32)))))"
        >>= (`shouldBe` "42")
    it "multi-param lambda" $
      run "(let ((f (lam ((x %INT) (y %INT)) (add x y)))) (print (int-to-str (f 20 22))))"
        >>= (`shouldBe` "42")
    it "higher-order function" $
      run (T.unlines
        [ "(let ((apply (lam (f x) (f x)))"
        , "      (double (lam ((x %INT)) (mul 2 x))))"
        , "  (print (int-to-str (apply double 21))))"
        ]) >>= (`shouldBe` "42")
    it "nested closures" $
      run (T.unlines
        [ "(let ((x 10))"
        , "  (let ((f (lam ((y %INT)) (lam ((z %INT)) (add x (add y z))))))"
        , "    (let ((g (f 20)))"
        , "      (print (int-to-str (g 12))))))"
        ]) >>= (`shouldBe` "42")

  describe "higher-kinded types" $ do
    it "HKT class with custom type" $
      run (T.unlines
        [ "(type Box (a) (MkBox a))"
        , "(cls BOXMAP () (f)"
        , "  (bmap %(-> a b) %(f a) %(f b)))"
        , "(inst BOXMAP %Box"
        , "  (bmap (lam ((fn %(-> a b)) (box %(Box a)))"
        , "    (case box ((MkBox x) (MkBox (fn x)))))))"
        , "(case (bmap (lam ((x %INT)) (add x 1)) (MkBox 41))"
        , "  ((MkBox y) (print (int-to-str y))))"
        ]) >>= (`shouldBe` "42")

    it "map with Maybe via FUNCTOR" $
      run (T.unlines
        [ "(cls FUNCTOR () (f)"
        , "  (map %(-> a b) %(f a) %(f b)))"
        , "(inst FUNCTOR %Maybe"
        , "  (map (lam ((fn %(-> a b)) (mx %(Maybe a)))"
        , "    (case mx ((Just x) (Just (fn x))) (_ Nothing)))))"
        , "(case (map (lam ((x %INT)) (add x 10)) (Just 32))"
        , "  ((Just y) (print (int-to-str y)))"
        , "  (_ (print \"nothing\")))"
        ]) >>= (`shouldBe` "42")

    it "map composed twice" $
      run (T.unlines
        [ "(cls FUNCTOR () (f)"
        , "  (map %(-> a b) %(f a) %(f b)))"
        , "(type Box (a) (MkBox a))"
        , "(inst FUNCTOR %Box"
        , "  (map (lam ((fn %(-> a b)) (box %(Box a)))"
        , "    (case box ((MkBox x) (MkBox (fn x)))))))"
        , "(let ((double (lam ((x %INT)) (mul x 2)))"
        , "      (inc    (lam ((x %INT)) (add x 1))))"
        , "  (case (map inc (map double (MkBox 20)))"
        , "    ((MkBox y) (print (int-to-str y)))))"
        ]) >>= (`shouldBe` "41")

    it "Monad-like bind with Maybe" $
      run (T.unlines
        [ "(cls MONAD () (m)"
        , "  (bind %(m a) %(-> a (m b)) %(m b)))"
        , "(inst MONAD %Maybe"
        , "  (bind (lam ((mx %(Maybe a)) (fn %(-> a (Maybe b))))"
        , "    (case mx"
        , "      ((Nothing) Nothing)"
        , "      ((Just x) (fn x))))))"
        , "(let ((safe-add (lam ((a %(Maybe %INT)) (b %(Maybe %INT)))"
        , "  (bind a (lam ((x %INT))"
        , "    (bind b (lam ((y %INT)) (Just (add x y)))))))))"
        , "  (case (safe-add (Just 20) (Just 22))"
        , "    ((Just z) (print (int-to-str z)))"
        , "    (_ (print \"nothing\"))))"
        ]) >>= (`shouldBe` "42")

    it "Monad bind short-circuits on Nothing" $
      run (T.unlines
        [ "(cls MONAD () (m)"
        , "  (bind %(m a) %(-> a (m b)) %(m b)))"
        , "(inst MONAD %Maybe"
        , "  (bind (lam ((mx %(Maybe a)) (fn %(-> a (Maybe b))))"
        , "    (case mx"
        , "      ((Nothing) Nothing)"
        , "      ((Just x) (fn x))))))"
        , "(let ((safe-div (lam ((a %INT) (b %INT))"
        , "  (if (eqi b 0) Nothing (Just (div a b))))))"
        , "  (case (bind (Just 10) (lam ((x %INT)) (safe-div x 0)))"
        , "    ((Just z) (print (int-to-str z)))"
        , "    (_ (print \"nothing\"))))"
        ]) >>= (`shouldBe` "nothing")

  describe "function type annotations" $ do
    it "annotated higher-order function works" $
      run (T.unlines
        [ "(let ((apply (lam ((f %(-> INT INT)) (x %INT)) (f x)))"
        , "      (inc (lam ((n %INT)) (add n 1))))"
        , "  (print (int-to-str (apply inc 41))))"
        ]) >>= (`shouldBe` "42")

  describe "partial application" $ do
    it "partially apply built-in" $
      run (T.unlines
        [ "(let ((inc (add 1)))"
        , "  (print (int-to-str (inc 41))))"
        ]) >>= (`shouldBe` "42")

    it "partially apply user function" $
      run (T.unlines
        [ "(let ((f (lam ((a %INT) (b %INT) (c %INT)) (add a (add b c)))))"
        , "  (let ((g (f 10)))"
        , "    (print (int-to-str (g 20 12)))))"
        ]) >>= (`shouldBe` "42")

    it "chain partial applications" $
      run (T.unlines
        [ "(let ((f (lam ((a %INT) (b %INT) (c %INT)) (add a (add b c)))))"
        , "  (let ((g (f 10)))"
        , "    (let ((h (g 20)))"
        , "      (print (int-to-str (h 12))))))"
        ]) >>= (`shouldBe` "42")

    it "partial apply constructor" $
      run (T.unlines
        [ "(type Pair () (MkPair %INT %INT))"
        , "(let ((mk (MkPair 1)))"
        , "  (case (mk 2)"
        , "    ((MkPair a b) (print (int-to-str (add a b))))))"
        ]) >>= (`shouldBe` "3")

    it "pass partial application as argument" $
      run (T.unlines
        [ "(let ((apply (lam (f (x %INT)) (f x))))"
        , "  (print (int-to-str (apply (add 1) 41))))"
        ]) >>= (`shouldBe` "42")

    it "partial apply used multiple times" $
      run (T.unlines
        [ "(let ((inc (add 1)))"
        , "  (progn (print (int-to-str (inc 10)))"
        , "         (print (int-to-str (inc 20)))"
        , "         (print (int-to-str (inc 30)))))"
        ]) >>= (`shouldBe` "11\n21\n31")

    it "supply 2 of 3 args" $
      run (T.unlines
        [ "(let ((f (lam ((a %INT) (b %INT) (c %INT)) (add a (add b c)))))"
        , "  (let ((g (f 10 20)))"
        , "    (print (int-to-str (g 12)))))"
        ]) >>= (`shouldBe` "42")

    it "partial apply float built-in" $
      run (T.unlines
        [ "(let ((add5 (addf 5.0)))"
        , "  (print (flt-to-str (add5 2.5))))"
        ]) >>= (`shouldBe` "7.5")

    it "over-application is still an error" $
      shouldFailToCompile
        "(add 1 2 3)"
        "different arities"

    it "partial apply with &opt" $
      run (T.unlines
        [ "(let ((f (lam ((a %INT) (b %INT) %opt (c 10)) (add a (add b c)))))"
        , "  (let ((g (f 1)))"
        , "    (print (int-to-str (g 2)))))"
        ]) >>= (`shouldBe` "13")

    it "partial apply with &opt, 2 required" $
      run (T.unlines
        [ "(let ((f (lam ((a %INT) (b %INT) %opt (c 100)) (add a (add b c)))))"
        , "  (let ((g (f 1)))"
        , "    (print (int-to-str (g 2)))))"
        ]) >>= (`shouldBe` "103")

    it "partial apply with &rest" $
      run (T.unlines
        [ "(let ((f (lam ((a %INT) (b %INT) &rest xs) (add a b))))"
        , "  (let ((g (f 10)))"
        , "    (print (int-to-str (g 20)))))"
        ]) >>= (`shouldBe` "30")

    it "partial apply with &key" $
      run (T.unlines
        [ "(let ((f (lam ((a %INT) (b %INT) &key (c 100)) (add a (add b c)))))"
        , "  (let ((g (f 1)))"
        , "    (print (int-to-str (g 2)))))"
        ]) >>= (`shouldBe` "103")

  describe "algebraic data types" $ do
    it "constructor and case match" $
      run (T.unlines
        [ "(type M (a) (N) (J a))"
        , "(let ((x (J 42)))"
        , "  (case x"
        , "    ((N) (print \"none\"))"
        , "    ((J v) (print (int-to-str v)))))"
        ]) >>= (`shouldBe` "42")
    it "zero-arg constructor" $
      run (T.unlines
        [ "(type M (a) (N) (J a))"
        , "(let ((x N))"
        , "  (case x"
        , "    ((N) (print \"nothing\"))"
        , "    ((J v) (print (int-to-str v)))))"
        ]) >>= (`shouldBe` "nothing")
    it "multiple constructors" $
      run (T.unlines
        [ "(type Color () (Red) (Green) (Blue))"
        , "(let ((c Green))"
        , "  (case c"
        , "    ((Red) (print \"r\"))"
        , "    ((Green) (print \"g\"))"
        , "    ((Blue) (print \"b\"))))"
        ]) >>= (`shouldBe` "g")

  describe "case expressions" $ do
    it "boolean case" $
      run "(case true (true (print \"yes\")) (false (print \"no\")))" >>= (`shouldBe` "yes")
    it "integer case" $
      run "(case 1 (1 (print \"one\")) (_ (print \"other\")))" >>= (`shouldBe` "one")
    it "wildcard case" $
      run "(case 42 (1 (print \"one\")) (_ (print \"other\")))" >>= (`shouldBe` "other")
    it "case with variable binding" $
      run "(case 42 (x (print (int-to-str x))))" >>= (`shouldBe` "42")

  describe "nested expressions" $ do
    it "nested arithmetic" $
      run "(print (int-to-str (add (mul 2 3) (sub 10 4))))" >>= (`shouldBe` "12")
    it "multiple top-level expressions" $
      run "(print \"first\") (print \"second\")" >>= (`shouldBe` "first\nsecond")

  describe "programs with no output" $ do
    it "no-op program" $ run "(add 1 2)" >>= (`shouldBe` "")
    it "unit literal" $ run "unit"        >>= (`shouldBe` "")

  -- EDGE CASES

  describe "arithmetic edge cases" $ do
    it "zero literal" $
      run "(print (int-to-str 0))" >>= (`shouldBe` "0")
    it "add zero" $
      run "(print (int-to-str (add 42 0)))" >>= (`shouldBe` "42")
    it "multiply by zero" $
      run "(print (int-to-str (mul 999 0)))" >>= (`shouldBe` "0")
    it "subtract self" $
      run "(print (int-to-str (sub 42 42)))" >>= (`shouldBe` "0")
    it "divide zero by nonzero" $
      run "(print (int-to-str (div 0 5)))" >>= (`shouldBe` "0")
    it "mod self" $
      run "(print (int-to-str (mod 7 7)))" >>= (`shouldBe` "0")
    it "negate zero" $
      run "(print (int-to-str (neg 0)))" >>= (`shouldBe` "0")
    it "double negation" $
      run "(print (int-to-str (neg (neg 42))))" >>= (`shouldBe` "42")
    it "large integer" $
      run "(print (int-to-str (mul 1000000 1000000)))" >>= (`shouldBe` "1000000000000")
    it "chained arithmetic" $
      run "(print (int-to-str (add (mul 2 (sub 10 3)) (div 100 (mod 7 4)))))"
        >>= (`shouldBe` "47")

  describe "float edge cases" $ do
    it "zero float" $
      run "(print (flt-to-str 0.0))" >>= (`shouldBe` "0")
    it "negate zero float" $
      run "(print (flt-to-str (negf 0.0)))" >>= (`shouldBe` "0")
    it "float subtract self" $
      run "(print (flt-to-str (subf 3.14 3.14)))" >>= (`shouldBe` "0")
    it "small float" $
      run "(print (flt-to-str 0.001))" >>= (`shouldBe` "0.001")
    it "large float" $
      run "(print (flt-to-str (mulf 1000000.0 1000000.0)))" >>= (`shouldBe` "1e+12")

  describe "comparison edge cases" $ do
    it "eq with zero" $
      run "(print (int-to-str (if (eqi 0 0) 1 0)))" >>= (`shouldBe` "1")
    it "lti same value" $
      run "(print (int-to-str (if (lti 5 5) 1 0)))" >>= (`shouldBe` "0")
    it "lei same value" $
      run "(print (int-to-str (if (lei 5 5) 1 0)))" >>= (`shouldBe` "1")
    it "gti same value" $
      run "(print (int-to-str (if (gti 5 5) 1 0)))" >>= (`shouldBe` "0")
    it "gei same value" $
      run "(print (int-to-str (if (gei 5 5) 1 0)))" >>= (`shouldBe` "1")
    it "eqf different" $
      run "(print (int-to-str (if (eqf 1.0 1.1) 1 0)))" >>= (`shouldBe` "0")
    it "gtf" $
      run "(print (int-to-str (if (gtf 2.0 1.0) 1 0)))" >>= (`shouldBe` "1")
    it "lef" $
      run "(print (int-to-str (if (lef 1.0 1.0) 1 0)))" >>= (`shouldBe` "1")
    it "gef" $
      run "(print (int-to-str (if (gef 1.0 1.0) 1 0)))" >>= (`shouldBe` "1")

  describe "boolean edge cases" $ do
    it "and ff" $
      run "(print (int-to-str (if (and false false) 1 0)))" >>= (`shouldBe` "0")
    it "or tt" $
      run "(print (int-to-str (if (or true true) 1 0)))" >>= (`shouldBe` "1")
    it "not true" $
      run "(print (int-to-str (if (not true) 1 0)))" >>= (`shouldBe` "0")
    it "double not" $
      run "(print (int-to-str (if (not (not true)) 1 0)))" >>= (`shouldBe` "1")
    it "chained and/or" $
      run "(print (int-to-str (if (or (and true false) (and true true)) 1 0)))"
        >>= (`shouldBe` "1")
    it "boolean in let binding" $
      run "(let ((b (eqi 1 1))) (print (int-to-str (if b 42 0))))" >>= (`shouldBe` "42")

  describe "string edge cases" $ do
    it "empty string" $
      run "(print \"\")" >>= (`shouldBe` "")
    it "empty string length" $
      run "(print (int-to-str (strlen \"\")))" >>= (`shouldBe` "0")
    it "concat with empty left" $
      run "(print (concat \"\" \"hello\"))" >>= (`shouldBe` "hello")
    it "concat with empty right" $
      run "(print (concat \"hello\" \"\"))" >>= (`shouldBe` "hello")
    it "concat both empty" $
      run "(print (concat \"\" \"\"))" >>= (`shouldBe` "")
    it "eqs empty strings" $
      run "(print (int-to-str (if (eqs \"\" \"\") 1 0)))" >>= (`shouldBe` "1")
    it "eqs empty vs nonempty" $
      run "(print (int-to-str (if (eqs \"\" \"a\") 1 0)))" >>= (`shouldBe` "0")
    it "strlen after concat" $
      run "(print (int-to-str (strlen (concat \"ab\" \"cde\"))))" >>= (`shouldBe` "5")
    it "multiple concats" $
      run "(print (concat (concat \"a\" \"b\") (concat \"c\" \"d\")))" >>= (`shouldBe` "abcd")
    it "string deduplication" $
      run "(print (concat \"x\" \"x\"))" >>= (`shouldBe` "xx")

  describe "conversion edge cases" $ do
    it "int-to-flt zero" $
      run "(print (flt-to-str (int-to-flt 0)))" >>= (`shouldBe` "0")
    it "flt-to-int zero" $
      run "(print (int-to-str (flt-to-int 0.0)))" >>= (`shouldBe` "0")
    it "flt-to-int truncates" $
      run "(print (int-to-str (flt-to-int 9.9)))" >>= (`shouldBe` "9")
    it "flt-to-int negative" $
      run "(print (int-to-str (flt-to-int (negf 3.7))))" >>= (`shouldBe` "-3")
    it "int-to-str negative" $
      run "(print (int-to-str (neg 1)))" >>= (`shouldBe` "-1")
    it "int-to-str zero" $
      run "(print (int-to-str 0))" >>= (`shouldBe` "0")
    it "roundtrip int-flt-int" $
      run "(print (int-to-str (flt-to-int (int-to-flt 42))))" >>= (`shouldBe` "42")

  describe "let binding edge cases" $ do
    it "variable shadowing" $
      run "(let ((x 1)) (let ((x 2)) (print (int-to-str x))))" >>= (`shouldBe` "2")
    it "outer var restored after shadow" $
      run (T.unlines
        [ "(let ((x 1))"
        , "  (let ((y (let ((x 99)) x)))"
        , "    (print (int-to-str (add x y)))))"
        ]) >>= (`shouldBe` "100")
    it "binding used multiple times" $
      run "(let ((x 5)) (print (int-to-str (add x (add x x)))))" >>= (`shouldBe` "15")
    it "deeply nested let" $
      run (T.unlines
        [ "(let ((a 1))"
        , "  (let ((b (add a 1)))"
        , "    (let ((c (add b 1)))"
        , "      (let ((d (add c 1)))"
        , "        (print (int-to-str d))))))"
        ]) >>= (`shouldBe` "4")
    it "let binding from if" $
      run "(let ((x (if true 42 0))) (print (int-to-str x)))" >>= (`shouldBe` "42")
    it "let binding from case" $
      run "(let ((x (case 5 (5 42) (_ 0)))) (print (int-to-str x)))" >>= (`shouldBe` "42")
    it "let with float binding" $
      run "(let ((x 3.14)) (print (flt-to-str x)))" >>= (`shouldBe` "3.14")
    it "let with string binding" $
      run "(let ((s \"hello\")) (print s))" >>= (`shouldBe` "hello")
    it "let with bool binding" $
      run "(let ((b true)) (print (int-to-str (if b 1 0))))" >>= (`shouldBe` "1")

  describe "if edge cases" $ do
    it "deeply nested if" $
      run "(print (int-to-str (if true (if true (if true (if false 0 42) 0) 0) 0)))"
        >>= (`shouldBe` "42")
    it "if as function argument" $
      run "(print (int-to-str (add (if true 10 0) (if false 0 32))))"
        >>= (`shouldBe` "42")
    it "if returning string" $
      run "(print (if true \"yes\" \"no\"))" >>= (`shouldBe` "yes")
    it "if returning float" $
      run "(print (flt-to-str (if true 1.5 2.5)))" >>= (`shouldBe` "1.5")
    it "if with comparison condition" $
      run "(print (int-to-str (if (gti (add 1 2) 2) 42 0)))" >>= (`shouldBe` "42")
    it "if branches with side effects" $
      run "(if true (print \"a\") (print \"b\"))" >>= (`shouldBe` "a")

  describe "case edge cases" $ do
    it "case with only wildcard" $
      run "(case 42 (_ (print \"catch-all\")))" >>= (`shouldBe` "catch-all")
    it "case with only variable" $
      run "(case 99 (n (print (int-to-str n))))" >>= (`shouldBe` "99")
    it "case inside if" $
      run "(print (int-to-str (if true (case 1 (1 42) (_ 0)) 0)))"
        >>= (`shouldBe` "42")
    it "if inside case" $
      run "(case true (true (print (int-to-str (if (eqi 1 1) 42 0)))) (false (print \"no\")))"
        >>= (`shouldBe` "42")
    it "case result in arithmetic" $
      run "(print (int-to-str (add (case 1 (1 10) (_ 0)) (case 2 (2 32) (_ 0)))))"
        >>= (`shouldBe` "42")
    it "boolean false case" $
      run "(case false (true (print \"yes\")) (false (print \"no\")))"
        >>= (`shouldBe` "no")
    it "integer case with multiple arms" $
      run "(case 3 (1 (print \"one\")) (2 (print \"two\")) (3 (print \"three\")) (_ (print \"other\")))"
        >>= (`shouldBe` "three")
    it "case falls through to wildcard" $
      run "(case 99 (1 (print \"one\")) (2 (print \"two\")) (_ (print \"other\")))"
        >>= (`shouldBe` "other")
    it "nested case" $
      run (T.unlines
        [ "(case 1"
        , "  (1 (case true"
        , "    (true (print \"1-true\"))"
        , "    (false (print \"1-false\"))))"
        , "  (_ (print \"other\")))"
        ]) >>= (`shouldBe` "1-true")

  describe "closure edge cases" $ do
    it "lambda returning bool" $
      run (T.unlines
        [ "(let ((is-zero (lam ((x %INT)) (eqi x 0))))"
        , "  (print (int-to-str (if (is-zero 0) 1 0))))"
        ]) >>= (`shouldBe` "1")
    it "lambda returning string" $
      run (T.unlines
        [ "(let ((greeting (lam ((name %STR)) (concat \"hello \" name))))"
        , "  (print (greeting \"world\")))"
        ]) >>= (`shouldBe` "hello world")
    it "lambda returning float" $
      run (T.unlines
        [ "(let ((half (lam ((x %FLT)) (divf x 2.0))))"
        , "  (print (flt-to-str (half 10.0))))"
        ]) >>= (`shouldBe` "5")
    it "closure called multiple times" $
      run (T.unlines
        [ "(let ((inc (lam ((x %INT)) (add x 1))))"
        , "  (print (int-to-str (add (inc 1) (inc 2)))))"
        ]) >>= (`shouldBe` "5")
    it "two closures sharing captured var" $
      run (T.unlines
        [ "(let ((base 10))"
        , "  (let ((f (lam ((x %INT)) (add base x)))"
        , "        (g (lam ((x %INT)) (mul base x))))"
        , "    (print (int-to-str (add (f 5) (g 3))))))"
        ]) >>= (`shouldBe` "45")
    it "closure capturing closure" $
      run (T.unlines
        [ "(let ((f (lam ((x %INT)) (add x 1))))"
        , "  (let ((g (lam ((y %INT)) (f (f y)))))"
        , "    (print (int-to-str (g 40)))))"
        ]) >>= (`shouldBe` "42")
    it "closure returning unit" $
      run (T.unlines
        [ "(let ((say (lam ((s %STR)) (print s))))"
        , "  (say \"hello\"))"
        ]) >>= (`shouldBe` "hello")
    it "identity lambda" $
      run "(let ((id (lam (x) x))) (print (int-to-str (id 42))))"
        >>= (`shouldBe` "42")

  describe "recursive closures" $ do
    it "recursive sum" $
      run (T.unlines
        [ "(let ((sum (lam ((n %INT))"
        , "  (if (eqi n 0) 0 (add n (sum (sub n 1)))))))"
        , "  (print (int-to-str (sum 5))))"
        ]) >>= (`shouldBe` "15")
    it "recursive countdown" $
      run (T.unlines
        [ "(let ((countdown (lam ((n %INT))"
        , "  (if (eqi n 0)"
        , "    (print \"done\")"
        , "    (let ((_ (print (int-to-str n))))"
        , "      (countdown (sub n 1)))))))"
        , "  (countdown 3))"
        ]) >>= (`shouldBe` "3\n2\n1\ndone")
    it "recursive with captured variable" $
      run (T.unlines
        [ "(let ((base 10))"
        , "  (let ((f (lam ((n %INT))"
        , "    (if (eqi n 0) base (f (sub n 1))))))"
        , "    (print (int-to-str (f 5)))))"
        ]) >>= (`shouldBe` "10")
    it "recursive factorial" $
      run (T.unlines
        [ "(let ((fact (lam ((n %INT))"
        , "  (if (eqi n 0) 1 (mul n (fact (sub n 1)))))))"
        , "  (print (int-to-str (fact 6))))"
        ]) >>= (`shouldBe` "720")
    it "recursive with dummy arg (loop pattern)" $
      run (T.unlines
        [ "(let ((n 3))"
        , "  (let ((loop (lam ((dummy %INT))"
        , "    (if (eqi n 0) unit"
        , "      (let ((_ (print (int-to-str n))))"
        , "        unit)))))"
        , "    (loop 0)))"
        ]) >>= (`shouldBe` "3")

  describe "ADT edge cases" $ do
    it "constructor inside lambda" $
      run (T.unlines
        [ "(type Box (a) (MkBox a))"
        , "(let ((wrap (lam ((x %INT)) (MkBox x))))"
        , "  (case (wrap 42)"
        , "    ((MkBox v) (print (int-to-str v)))))"
        ]) >>= (`shouldBe` "42")
    it "constructor with two fields" $
      run (T.unlines
        [ "(type Pair (a b) (MkPair a b))"
        , "(let ((p (MkPair 10 32)))"
        , "  (case p"
        , "    ((MkPair x y) (print (int-to-str (add x y))))))"
        ]) >>= (`shouldBe` "42")
    it "multiple types in same program" $
      run (T.unlines
        [ "(type Bool2 () (T) (F))"
        , "(type M (a) (N) (J a))"
        , "(let ((b T) (m (J 42)))"
        , "  (case b"
        , "    ((T) (case m"
        , "      ((N) (print \"none\"))"
        , "      ((J v) (print (int-to-str v)))))"
        , "    ((F) (print \"false\"))))"
        ]) >>= (`shouldBe` "42")
    it "ADT inside ADT" $
      run (T.unlines
        [ "(type M (a) (N) (J a))"
        , "(let ((x (J (J 42))))"
        , "  (case x"
        , "    ((N) (print \"outer-none\"))"
        , "    ((J inner) (case inner"
        , "      ((N) (print \"inner-none\"))"
        , "      ((J v) (print (int-to-str v)))))))"
        ]) >>= (`shouldBe` "42")
    it "mixed-arity constructor arms" $
      run (T.unlines
        [ "(type Shape () (Circle %FLT) (Rect %FLT %FLT) (Point))"
        , "(let ((s (Rect 3.0 4.0)))"
        , "  (case s"
        , "    ((Circle r) (print (flt-to-str r)))"
        , "    ((Rect w h) (print (flt-to-str (mulf w h))))"
        , "    ((Point) (print \"point\"))))"
        ]) >>= (`shouldBe` "12")
    it "zero-arg constructor used twice" $
      run (T.unlines
        [ "(type M (a) (N) (J a))"
        , "(let ((x N) (y N))"
        , "  (case x"
        , "    ((N) (case y"
        , "      ((N) (print \"both-none\"))"
        , "      ((J v) (print \"unreachable\"))))"
        , "    ((J v) (print \"unreachable\"))))"
        ]) >>= (`shouldBe` "both-none")
    it "constructor in let binding" $
      run (T.unlines
        [ "(type M (a) (N) (J a))"
        , "(let ((x (J 42)))"
        , "  (let ((val (case x"
        , "    ((N) 0)"
        , "    ((J v) v))))"
        , "    (print (int-to-str val))))"
        ]) >>= (`shouldBe` "42")

  describe "complex integration" $ do
    it "fibonacci-style chain" $
      run (T.unlines
        [ "(let ((a 1) (b 1))"
        , "  (let ((c (add a b)))"
        , "    (let ((d (add b c)))"
        , "      (let ((e (add c d)))"
        , "        (print (int-to-str e))))))"
        ]) >>= (`shouldBe` "5")
    it "map-like pattern" $
      run (T.unlines
        [ "(let ((f (lam ((x %INT)) (mul x x))))"
        , "  (print (int-to-str (add (f 3) (f 4)))))"
        ]) >>= (`shouldBe` "25")
    it "closure as data in ADT" $
      run (T.unlines
        [ "(type M (a) (N) (J a))"
        , "(let ((f (lam ((x %INT)) (add x 1))))"
        , "  (let ((wrapped (J f)))"
        , "    (case wrapped"
        , "      ((N) (print \"none\"))"
        , "      ((J g) (print (int-to-str (g 41)))))))"
        ]) >>= (`shouldBe` "42")
    it "three top-level prints" $
      run "(print \"a\") (print \"b\") (print \"c\")" >>= (`shouldBe` "a\nb\nc")
    it "let and if and case combined" $
      run (T.unlines
        [ "(let ((x (if (eqi 1 1) 5 0)))"
        , "  (case x"
        , "    (5 (print \"five\"))"
        , "    (_ (print \"other\"))))"
        ]) >>= (`shouldBe` "five")

  describe "new built-ins" $ do
    it "str-contains found" $
      run "(print (int-to-str (if (str-contains \"hello world\" \"world\") 1 0)))"
        >>= (`shouldBe` "1")
    it "str-contains not found" $
      run "(print (int-to-str (if (str-contains \"hello world\" \"xyz\") 1 0)))"
        >>= (`shouldBe` "0")
    it "str-contains empty needle" $
      run "(print (int-to-str (if (str-contains \"hello\" \"\") 1 0)))"
        >>= (`shouldBe` "1")
    it "str-contains empty haystack" $
      run "(print (int-to-str (if (str-contains \"\" \"a\") 1 0)))"
        >>= (`shouldBe` "0")
    it "substr" $
      run "(print (substr \"hello world\" 6 5))" >>= (`shouldBe` "world")
    it "substr from start" $
      run "(print (substr \"hello\" 0 3))" >>= (`shouldBe` "hel")
    it "substr zero length" $
      run "(print (concat \"[\" (concat (substr \"hello\" 0 0) \"]\")))"
        >>= (`shouldBe` "[]")
    it "argc" $
      run "(print (int-to-str (argc unit)))" >>= (`shouldBe` "1")
    it "argv 0" $
      runWith ["foo", "bar"] "(print (argv 1))" >>= (`shouldBe` "foo")
    it "read-line and is-eof" $
      runWithInput "hello\n" (T.unlines
        [ "(let ((line (read-line unit)))"
        , "  (print line))"
        ]) >>= (`shouldBe` "hello")
    it "is-eof after exhausted input" $
      runWithInput "" (T.unlines
        [ "(let ((line (read-line unit)))"
        , "  (print (int-to-str (if (is-eof unit) 1 0))))"
        ]) >>= (`shouldBe` "1")
    it "read-line multiple lines" $
      runWithInput "first\nsecond\n" (T.unlines
        [ "(let ((a (read-line unit)))"
        , "  (let ((b (read-line unit)))"
        , "    (let ((_ (print a)))"
        , "      (print b))))"
        ]) >>= (`shouldBe` "first\nsecond")
    it "is-eof false before exhaustion" $
      runWithInput "hello\n" (T.unlines
        [ "(let ((_ (print (int-to-str (if (is-eof unit) 1 0)))))"
        , "  (let ((line (read-line unit)))"
        , "    (print line)))"
        ]) >>= (`shouldBe` "0\nhello")
    it "argc with extra args" $
      runWith ["x", "y"] "(print (int-to-str (argc unit)))" >>= (`shouldBe` "3")
    it "argv second arg" $
      runWith ["hello", "world"] "(print (argv 2))" >>= (`shouldBe` "world")
    it "substr full string" $
      run "(print (substr \"hello\" 0 5))" >>= (`shouldBe` "hello")
    it "str-contains self" $
      run "(print (int-to-str (if (str-contains \"hello\" \"hello\") 1 0)))"
        >>= (`shouldBe` "1")

  describe "CLI stdlib module" $ do
    it "uses the namespaced pll-exit runtime helper" $ do
      cliSrc <- T.IO.readFile "stdlib/CLI.pll"
      ir <- multiModulePipeline [("CLI", cliSrc)] (T.unlines
        [ "(import CLI)"
        , "(CLI"
        , "  (:flag :verbose \"-v\" \"--verbose\"))"
        , "(print \"ok\")"
        ])
      T.isInfixOf "call void @pll_ffi_call(ptr @pll_exit" ir `shouldBe` True
      T.isInfixOf "call void @pll_ffi_call(ptr @exit" ir `shouldBe` False

    it "binds flags, options, args, and rest arguments with one top-level CLI form" $
      runWithCLIArgs ["--verbose", "-o", "build/out.txt", "fast", "input.pll", "extra-1", "extra-2"]
        (T.unlines
          [ "(import CLI)"
          , "(fun list-len ((xs %(List %STR))) %INT"
          , "  (case xs"
          , "    ((Empty) 0)"
          , "    ((Cons _ rest) (add 1 (list-len rest)))))"
          , "(CLI"
          , "  (:flag :verbose \"-v\" \"--verbose\")"
          , "  (:option :output \"-o\" \"--output\")"
          , "  (:arg :mode)"
          , "  (:arg :input)"
          , "  (:rest :extras))"
          , "(print (if verbose \"true\" \"false\"))"
          , "(case output"
          , "  ((Just out) (print out))"
          , "  (_ (print \"missing\")))"
          , "(print mode)"
          , "(print input)"
          , "(print (int-to-str (list-len extras)))"
          ])
        >>= (`shouldBe` "true\nbuild/out.txt\nfast\ninput.pll\n2")

    it "parses argv from the real process command line" $
      runWithCLIArgs ["fast", "main.pll"]
        (T.unlines
          [ "(import CLI)"
          , "(CLI"
          , "  (:arg :mode)"
          , "  (:arg :input))"
          , "(print mode)"
          , "(print input)"
          ])
        >>= (`shouldBe` "fast\nmain.pll")

    it "reports unknown options as parse errors" $
      runWithCLIArgsRaw ["--bogus"]
        (T.unlines
          [ "(import CLI)"
          , "(CLI"
          , "  (:flag :verbose \"-v\" \"--verbose\"))"
          , "(print \"unexpected\")"
          ])
        >>= (\(ec, out, _) -> do
              ec `shouldBe` ExitFailure 1
              out `shouldContain` "unknown option: --bogus")

    it "allows multiple CLI declarations in one file" $
      runWithCLIArgs ["--verbose"]
        (T.unlines
          [ "(import CLI)"
          , "(CLI"
          , "  (:flag :verbose-a \"-v\" \"--verbose\"))"
          , "(CLI"
          , "  (:flag :verbose-b \"-v\" \"--verbose\"))"
          , "(print (if verbose-a \"true\" \"false\"))"
          , "(print (if verbose-b \"true\" \"false\"))"
          ])
        >>= (`shouldBe` "true\ntrue")

    it "advanced CLI example handles flags, options, and -- for rest args" $ do
      src <- T.IO.readFile "example-programs/modules/valid/cli-plan/main.pllisp"
      runWithCLIArgs
        [ "--verbose"
        , "--dry-run"
        , "--profile", "release"
        , "-o", "dist/app.bundle"
        , "--"
        , "src/main.pll"
        , "-not-an-option"
        ]
        src
        >>= (`shouldBe` "release\ndist/app.bundle\n2\nverbose\ndry-run\nsrc/main.pll")

  describe "built-in List type" $ do
    it "constructs Empty" $
      run "(case Empty ((Empty) (print \"empty\")))" >>= (`shouldBe` "empty")

    it "constructs Cons and pattern matches" $
      run (T.unlines
        [ "(let ((xs (Cons 1 (Cons 2 (Cons 3 Empty)))))"
        , "  (case xs"
        , "    ((Cons h _) (print (int-to-str h)))))"
        ]) >>= (`shouldBe` "1")

    it "recursive list traversal" $
      run (T.unlines
        [ "(let ((sum (lam ((xs %(List %INT))) (case xs"
        , "    ((Empty) 0)"
        , "    ((Cons h t) (add h (sum t)))))))"
        , "  (print (int-to-str (sum (Cons 10 (Cons 20 (Cons 30 Empty)))))))"
        ]) >>= (`shouldBe` "60")

  describe "rx" $ do
    it "rx-match positive" $
      run "(print (int-to-str (if (rx-match /^[0-9]+$/ \"12345\") 1 0)))"
        >>= (`shouldBe` "1")

    it "rx-match negative" $
      run "(print (int-to-str (if (rx-match /^[0-9]+$/ \"abc\") 1 0)))"
        >>= (`shouldBe` "0")

    it "rx-find extracts first match" $
      run "(print (rx-find /[0-9]+/ \"foo123bar456\"))"
        >>= (`shouldBe` "123")

    it "rx-find no match returns empty string" $
      run "(print (concat \"[\" (concat (rx-find /[0-9]+/ \"abc\") \"]\")))"
        >>= (`shouldBe` "[]")

    it "rx-sub replaces first match" $
      run "(print (rx-sub /[0-9]+/ \"X\" \"a1b2c3\"))"
        >>= (`shouldBe` "aXb2c3")

    it "rx-gsub replaces all matches" $
      run "(print (rx-gsub /[0-9]+/ \"X\" \"a1b2c3\"))"
        >>= (`shouldBe` "aXbXcX")

    it "rx-sub with backreference" $
      run "(print (rx-sub /(\\w+)@(\\w+)/ \"$2/$1\" \"user@host\"))"
        >>= (`shouldBe` "host/user")

    it "rx-split on comma" $ do
      run (T.unlines
        [ "(let ((parts (rx-split /,/ \"a,b,c\")))"
        , "  (case parts"
        , "    ((Cons a _) (print a))))"
        ]) >>= (`shouldBe` "a")

    it "rx-split prints all parts" $ do
      run (T.unlines
        [ "(let ((print-list (lam ((xs %(List %STR))) (case xs"
        , "    ((Empty) unit)"
        , "    ((Cons h t) (let ((_ (print h))) (print-list t)))))))"
        , "  (print-list (rx-split /,/ \"x,y,z\")))"
        ]) >>= (`shouldBe` "x\ny\nz")

    it "rx-captures extracts groups" $ do
      run (T.unlines
        [ "(let ((caps (rx-captures /(\\w+)@(\\w+)/ \"user@host\")))"
        , "  (case caps"
        , "    ((Cons user rest) (case rest"
        , "      ((Cons host _) (print (concat user (concat \"@\" host))))))))"
        ]) >>= (`shouldBe` "user@host")

    it "rx-captures no match returns Empty" $ do
      run (T.unlines
        [ "(case (rx-captures /(\\d+)/ \"abc\")"
        , "  ((Empty) (print \"none\"))"
        , "  ((Cons _ _) (print \"found\")))"
        ]) >>= (`shouldBe` "none")

    it "rx with case-insensitive flag" $
      run "(print (int-to-str (if (rx-match /hello/i \"HELLO\") 1 0)))"
        >>= (`shouldBe` "1")

    it "rx stored in let binding" $
      run "(let ((re /[0-9]+/)) (print (rx-find re \"abc123\")))"
        >>= (`shouldBe` "123")

    it "rx-compile from string" $
      run "(let ((re (rx-compile \"[0-9]+\"))) (print (rx-find re \"abc123\")))"
        >>= (`shouldBe` "123")

    it "rx-compile used in match" $
      run "(let ((re (rx-compile \"^hello$\"))) (print (int-to-str (if (rx-match re \"hello\") 1 0))))"
        >>= (`shouldBe` "1")

    it "rx with \\Q\\E literal quoting" $
      run "(print (int-to-str (if (rx-match /\\Qfoo.bar\\E/ \"foo.bar\") 1 0)))"
        >>= (`shouldBe` "1")

    it "rx \\Q\\E does not match metachar" $
      run "(print (int-to-str (if (rx-match /\\Qfoo.bar\\E/ \"fooXbar\") 1 0)))"
        >>= (`shouldBe` "0")

  describe "prelude special import" $ do
    it "prelude constructors available unqualified" $ do
      run "(case Empty ((Empty) (print \"ok\")))" >>= (`shouldBe` "ok")

    it "prelude constructors available qualified as PRELUDE.X" $ do
      run "(case PRELUDE.Empty ((PRELUDE.Empty) (print \"ok\")))" >>= (`shouldBe` "ok")

    it "qualified constructor with arguments in pattern" $ do
      run (T.unlines
        [ "(let ((xs (PRELUDE.Cons 1 PRELUDE.Empty)))"
        , "  (case xs ((PRELUDE.Cons h _) (print (int-to-str h)))))"
        ]) >>= (`shouldBe` "1")

    it "user module with same constructor name does not collide (qualified only)" $ do
      let modSrc = "(type Box () (Empty))"
          mainSrc = T.unlines
            [ "# prelude Empty and Mod.Empty coexist"
            , "(progn"
            , "  (case Empty ((Empty) (print \"prelude\")))"
            , "  (case Mod.Empty ((Mod.Empty) (print \"mod\"))))"
            ]
      runWithModule "MOD" modSrc [] mainSrc >>= (`shouldBe` "prelude\nmod")

  describe "module imports" $ do
    it "unqualified import" $ do
      let modSrc = "(let ((square (lam ((x %INT)) (mul x x)))) unit)"
          mainSrc = "(print (int-to-str (square 5)))"
      runWithModule "MOD" modSrc ["SQUARE"] mainSrc >>= (`shouldBe` "25")

    it "qualified import" $ do
      let modSrc = "(let ((square (lam ((x %INT)) (mul x x)))) unit)"
          mainSrc = "(print (int-to-str (Mod.square 7)))"
      runWithModule "MOD" modSrc [] mainSrc >>= (`shouldBe` "49")

    it "aliased import uses alias as qualifier" $ do
      let modSrc = "(let ((square (lam ((x %INT)) (mul x x)))) unit)"
          mainSrc = "(import MOD M)\n(print (int-to-str (M.square 7)))"
      runWithModules [("MOD", modSrc)] mainSrc >>= (`shouldBe` "49")

    it "imported type constructors" $ do
      let modSrc = "(type Box (a) (MkBox a))"
          mainSrc = T.unlines
            [ "(let ((b (MkBox 42)))"
            , "  (case b ((MkBox x) (print (int-to-str x)))))"
            ]
      runWithModule "MOD" modSrc ["MKBOX"] mainSrc >>= (`shouldBe` "42")

    it "qualified constructor in pattern" $ do
      let modSrc = "(type Box (a) (MkBox a))"
          mainSrc = T.unlines
            [ "(let ((b (Mod.MkBox 42)))"
            , "  (case b ((Mod.MkBox x) (print (int-to-str x)))))"
            ]
      runWithModule "MOD" modSrc [] mainSrc >>= (`shouldBe` "42")

    it "multiple bindings from one module" $ do
      let modSrc = T.unlines
            [ "(let ((double (lam ((x %INT)) (mul 2 x)))"
            , "      (square (lam ((x %INT)) (mul x x))))"
            , "  unit)"
            ]
          mainSrc = "(print (int-to-str (square (double 3))))"
      runWithModule "MOD" modSrc ["SQUARE", "DOUBLE"] mainSrc >>= (`shouldBe` "36")

    it "imported macro" $ do
      let modSrc = T.unlines
            [ "(mac double-print (x)"
            , "  `(progn (print ,x) (print ,x)))"
            ]
          mainSrc = "(double-print \"hi\")"
      runWithModule "MOD" modSrc [] mainSrc >>= (`shouldBe` "hi\nhi")

    it "imported macro uses prelude macro" $ do
      let modSrc = T.unlines
            [ "(fun square ((x %INT)) (mul x x))"
            ]
          mainSrc = "(print (int-to-str (square 6)))"
      runWithModule "MOD" modSrc ["SQUARE"] mainSrc >>= (`shouldBe` "36")

    it "imported macro with values" $ do
      let modSrc = T.unlines
            [ "(let ((helper (lam ((x %INT)) (mul x x)))) unit)"
            , "(mac apply-helper (x) `(helper ,x))"
            ]
          mainSrc = "(print (int-to-str (apply-helper 5)))"
      runWithModule "MOD" modSrc ["HELPER"] mainSrc >>= (`shouldBe` "25")

    it "macros can use earlier runtime function definitions from the same module" $ do
      let src = T.unlines
            [ "(fun double-int ((x %INT)) (add x x))"
            , "(mac double-lit (x)"
            , "  (syntax-int (double-int (syntax-int-value x))))"
            , "(print (int-to-str (double-lit 21)))"
            ]
      runWithModules [] src >>= (`shouldBe` "42")

    it "imported procedural macro" $ do
      let modSrc = T.unlines
            [ "(mac my-do (&rest args)"
            , "  (if (eq (syntax-length args) 1)"
            , "    (syntax-car args)"
            , "    `(let ((_ ,(syntax-car args))) (my-do ,@(syntax-cdr args)))))"
            ]
          mainSrc = "(my-do (print \"a\") (print \"b\") (print \"c\"))"
      runWithModule "MOD" modSrc [] mainSrc >>= (`shouldBe` "a\nb\nc")

    it "imported macro library can use compile-time helper bindings" $ do
      let modSrc = T.unlines
            [ "(eval-when (:compile-toplevel)"
            , "  (let ((emit-double (lam (x) `(add ,x ,x))))"
            , "    emit-double))"
            , "(mac double (x) (emit-double x))"
            ]
          mainSrc = "(print (int-to-str (double 21)))"
      runWithModule "MOD" modSrc [] mainSrc >>= (`shouldBe` "42")

    it "imported macro libraries can capture earlier runtime function definitions" $ do
      let modSrc = T.unlines
            [ "(fun double-int ((x %INT)) (add x x))"
            , "(mac double-lit (x)"
            , "  (syntax-int (double-int (syntax-int-value x))))"
            ]
          mainSrc = "(print (int-to-str (double-lit 21)))"
      runWithModule "MOD" modSrc [] mainSrc >>= (`shouldBe` "42")

    it "imported macro libraries can capture pure runtime helpers from imported modules" $ do
      let modBase = T.unlines
            [ "(fun double-int ((x %INT)) (add x x))"
            ]
          modLib = T.unlines
            [ "(import BASE)"
            , "(mac double-lit (x)"
            , "  (syntax-int (double-int (syntax-int-value x))))"
            ]
          mainSrc = T.unlines
            [ "(import LIB)"
            , "(print (int-to-str (double-lit 21)))"
            ]
      runWithModules [("BASE", modBase), ("LIB", modLib)] mainSrc >>= (`shouldBe` "42")

    it "aliased macro-providing modules still expose macros" $ do
      let modSrc = "(mac double (x) `(add ,x ,x))"
          mainSrc = T.unlines
            [ "(import MOD M)"
            , "(print (int-to-str (double 21)))"
            ]
      runWithModules [("MOD", modSrc)] mainSrc >>= (`shouldBe` "42")

    it "selective imports still load macros from macro-providing modules" $ do
      let modSrc = T.unlines
            [ "(let ((square (lam ((x %INT)) (mul x x)))) unit)"
            , "(mac double (x) `(add ,x ,x))"
            ]
          mainSrc = T.unlines
            [ "(import MOD (square))"
            , "(print (int-to-str (add (square 3) (double 21))))"
            ]
      runWithModules [("MOD", modSrc)] mainSrc >>= (`shouldBe` "51")

    it "imported compile-time helper bindings are visible in importing macro modules" $ do
      let modBase = T.unlines
            [ "(eval-when (:compile-toplevel)"
            , "  (let ((emit-double (lam (x) `(add ,x ,x))))"
            , "    emit-double))"
            ]
          modLib = T.unlines
            [ "(import BASE)"
            , "(mac double (x) (emit-double x))"
            ]
          mainSrc = T.unlines
            [ "(import LIB)"
            , "(print (int-to-str (double 21)))"
            ]
      runWithModules [("BASE", modBase), ("LIB", modLib)] mainSrc >>= (`shouldBe` "42")

    it "imported macro libraries can use typed compile-time helpers with ADTs" $ do
      let modLib = T.unlines
            [ "(eval-when (:compile-toplevel)"
            , "  (let ((default (Just 42)))"
            , "    default)"
            , "  (fun emit-default ()"
            , "    (case default"
            , "      ((Just x) `,x)"
            , "      (_ `0))))"
            , "(mac use-default () (emit-default))"
            ]
          mainSrc = T.unlines
            [ "(import LIB)"
            , "(print (int-to-str (use-default)))"
            ]
      runWithModules [("LIB", modLib)] mainSrc >>= (`shouldBe` "42")

    it "imported macro libraries can use their own earlier runtime type declarations" $ do
      let modLib = T.unlines
            [ "(type Flag () (Flag))"
            , "(eval-when (:compile-toplevel)"
            , "  (let ((default Flag))"
            , "    default)"
            , "  (fun emit-flag ()"
            , "    (case default"
            , "      ((Flag) `7))))"
            , "(mac use-flag () (emit-flag))"
            ]
          mainSrc = T.unlines
            [ "(import LIB)"
            , "(print (int-to-str (use-flag)))"
            ]
      runWithModules [("LIB", modLib)] mainSrc >>= (`shouldBe` "7")

    it "transitively imported macros remain available during recursive expansion" $ do
      let modBase = "(mac double (x) `(add ,x ,x))"
          modLib = T.unlines
            [ "(import BASE)"
            , "(mac quadruple (x) `(double (double ,x)))"
            ]
          mainSrc = T.unlines
            [ "(import LIB)"
            , "(print (int-to-str (quadruple 21)))"
            ]
      runWithModules [("BASE", modBase), ("LIB", modLib)] mainSrc >>= (`shouldBe` "84")

    it "rejects duplicate imported compile-time helpers" $
      shouldFailToCompileModules
        [ ("A", T.unlines
            [ "(eval-when (:compile-toplevel)"
            , "  (let ((emit-double (lam (x) `(add ,x ,x))))"
            , "    emit-double))"
            ])
        , ("B", T.unlines
            [ "(eval-when (:compile-toplevel)"
            , "  (let ((emit-double (lam (x) `(mul ,x 2))))"
            , "    emit-double))"
            ])
        ]
        (T.unlines
          [ "(import A)"
          , "(import B)"
          , "unit"
          ])
        "compile-time helper collision"

    it "rejects duplicate imported macros" $
      shouldFailToCompileModules
        [ ("A", "(mac double (x) `(add ,x ,x))")
        , ("B", "(mac double (x) `(mul ,x 2))")
        ]
        (T.unlines
          [ "(import A)"
          , "(import B)"
          , "(print (int-to-str (double 21)))"
          ])
        "duplicate imported macro definition"

  describe "recursive module imports" $ do
    it "chained imports" $ do
      let modB = "(let ((double (lam ((x %INT)) (mul 2 x)))) unit)"
          modA = T.unlines
            [ "(import B (double))"
            , "(let ((quad (lam ((x %INT)) (double (double x))))) unit)"
            ]
          mainSrc = T.unlines
            [ "(import A (quad))"
            , "(print (int-to-str (quad 3)))"
            ]
      runWithModules [("B", modB), ("A", modA)] mainSrc >>= (`shouldBe` "12")

    it "diamond imports" $ do
      let modC = "(let ((inc (lam ((x %INT)) (add x 1)))) unit)"
          modB = T.unlines
            [ "(import C (inc))"
            , "(let ((inc2 (lam ((x %INT)) (inc (inc x))))) unit)"
            ]
          modA = T.unlines
            [ "(import C (inc))"
            , "(let ((inc3 (lam ((x %INT)) (inc (inc (inc x)))))) unit)"
            ]
          mainSrc = T.unlines
            [ "(import A (inc3))"
            , "(import B (inc2))"
            , "(print (int-to-str (add (inc2 0) (inc3 0))))"
            ]
      runWithModules [("C", modC), ("A", modA), ("B", modB)] mainSrc >>= (`shouldBe` "5")

    it "transitive type" $ do
      let modBase = "(type Pair (a b) (MkPair a b))"
          modLib = T.unlines
            [ "(import BASE (MkPair))"
            , "(let ((make-pair (lam ((x %INT) (y %INT)) (MkPair x y)))) unit)"
            ]
          mainSrc = T.unlines
            [ "(import BASE (MkPair))"
            , "(import LIB (make-pair))"
            , "(let ((p (make-pair 10 32)))"
            , "  (case p"
            , "    ((MkPair x y) (print (int-to-str (add x y))))))"
            ]
      runWithModules [("BASE", modBase), ("LIB", modLib)] mainSrc >>= (`shouldBe` "42")

    it "transitive macro" $ do
      let modBase = "(mac twice (x) `(progn ,x ,x))"
          modLib = T.unlines
            [ "(import BASE)"
            , "(mac quad (x) `(twice (twice ,x)))"
            ]
          mainSrc = T.unlines
            [ "(import LIB)"
            , "(quad (print \"x\"))"
            ]
      runWithModules [("BASE", modBase), ("LIB", modLib)] mainSrc >>= (`shouldBe` "x\nx\nx\nx")

    it "imports val and var bindings exported from another module" $ do
      let modState = T.unlines
            [ "(module STATE)"
            , "(val banner \"ready\")"
            , "(var counter 41)"
            ]
          mainSrc = T.unlines
            [ "(import STATE (banner counter))"
            , "(print banner)"
            , "(let ((_ (set! counter (add (deref counter) 1))))"
            , "  (print (int-to-str (deref counter))))"
            ]
      runWithModules [("STATE", modState)] mainSrc >>= (`shouldBe` "ready\n42")

    it "rejects set! on an imported val binding" $
      shouldFailToCompileModules
        [ ("STATE", T.unlines
            [ "(module STATE)"
            , "(val banner \"ready\")"
            , "(var counter 0)"
            ])
        ]
        (T.unlines
          [ "(import STATE (banner counter))"
          , "(let ((_ (set! counter 1)))"
          , "  (set! banner \"nope\"))"
          ])
        "cannot unify"

  describe "macros" $ do
    it "when macro" $ do
      run "(when true (print \"yes\"))"
        >>= (`shouldBe` "yes")

    it "when macro false branch" $ do
      run (T.unlines
        [ "(when false (print \"no\"))"
        , "(print \"done\")"
        ]) >>= (`shouldBe` "done")

    it "unless macro" $ do
      run "(unless false (print \"ran\"))"
        >>= (`shouldBe` "ran")

    it "do macro sequences expressions" $ do
      run (T.unlines
        [ "(mac do (&rest args)"
        , "  (if (eq (syntax-length args) 1)"
        , "    (syntax-car args)"
        , "    `(let ((_ ,(syntax-car args))) (do ,@(syntax-cdr args)))))"
        , "(do (print \"a\") (print \"b\") (print \"c\"))"
        ]) >>= (`shouldBe` "a\nb\nc")

    it "cond macro" $ do
      run (T.unlines
        [ "(let ((x 2))"
        , "  (cond ((eqi x 1) (print \"one\"))"
        , "        ((eqi x 2) (print \"two\"))"
        , "        ((eqi x 3) (print \"three\"))))"
        ]) >>= (`shouldBe` "two")

    it "macro using another macro" $ do
      run (T.unlines
        [ "(mac my-unless (test body) `(if ,test unit ,body))"
        , "(mac my-when (test body) `(my-unless (not ,test) ,body))"
        , "(my-when true (print \"ok\"))"
        ]) >>= (`shouldBe` "ok")

    it "fun macro accepts an explicit return annotation" $ do
      run (T.unlines
        [ "(fun inc ((x %INT)) %INT"
        , "  (add x 1))"
        , "(print (int-to-str (inc 41)))"
        ]) >>= (`shouldBe` "42")

    it "fun macro rejects a bare return annotation with no body" $
      shouldFailToCompile
        (T.unlines
          [ "(fun broken ((x %INT)) %INT)"
          , "(print (int-to-str (broken 41)))"
          ])
        "fun expects"

    it "fun macro accepts multiple body forms" $ do
      run (T.unlines
        [ "(fun greet ((name %STR))"
        , "  (print \"greeting\")"
        , "  (print (concat \"hello \" name)))"
        , "(greet \"pllisp\")"
        ]) >>= (`shouldBe` "greeting\nhello pllisp")

    it "fun macro accepts multiple body forms with an explicit return annotation" $ do
      run (T.unlines
        [ "(fun next ((x %INT)) %INT"
        , "  (print \"bump\")"
        , "  (add x 1))"
        , "(print (int-to-str (next 41)))"
        ]) >>= (`shouldBe` "bump\n42")

    it "val introduces a top-level immutable binding" $ do
      run (T.unlines
        [ "(val answer 42)"
        , "(print (int-to-str answer))"
        ]) >>= (`shouldBe` "42")

    it "var introduces a top-level mutable ref binding" $ do
      run (T.unlines
        [ "(var counter 0)"
        , "(let ((_ (set! counter 41)))"
        , "  (print (int-to-str (add (deref counter) 1))))"
        ]) >>= (`shouldBe` "42")

    it "macro with type annotations" $ do
      run (T.unlines
        [ "(mac typed-id (x) `,x)"
        , "(print (int-to-str (typed-id (add 1 2))))"
        ]) >>= (`shouldBe` "3")

    it "macro in nested position" $ do
      run (T.unlines
        [ "(mac double (x) `(add ,x ,x))"
        , "(print (int-to-str (double 21)))"
        ]) >>= (`shouldBe` "42")

    it "macro with destructuring params" $ do
      run (T.unlines
        [ "(mac first-of ((a b)) `,a)"
        , "(print (int-to-str (first-of (42 99))))"
        ]) >>= (`shouldBe` "42")

    it "macro-introduced let bindings do not capture caller identifiers" $ do
      run (T.unlines
        [ "(mac with-temp (val body)"
        , "  `(let ((tmp ,val)) ,body))"
        , "(let ((tmp 99))"
        , "  (print (int-to-str (with-temp 1 tmp))))"
        ]) >>= (`shouldBe` "99")

    it "macro-introduced references stay linked to their own bindings" $ do
      run (T.unlines
        [ "(mac return-temp (val)"
        , "  `(let ((tmp ,val)) tmp))"
        , "(let ((tmp 99))"
        , "  (print (int-to-str (return-temp 1))))"
        ]) >>= (`shouldBe` "1")

    it "syntax-raw-symbol allows intentional top-level bindings" $ do
      run (T.unlines
        [ "(mac define-result (x)"
        , "  `(let ((,(syntax-raw-symbol \"result\") ,x)) unit))"
        , "(define-result 42)"
        , "(print (int-to-str result))"
        ]) >>= (`shouldBe` "42")

  describe "if-let / when-let / unless-let" $ do
    it "if-let on truthy Maybe" $ do
      run (T.unlines
        [ "(let ((x (Just 42)))"
        , "  (if-let (y x) (print \"yes\") (print \"no\")))"
        ]) >>= (`shouldBe` "yes")

    it "if-let on falsy Maybe" $ do
      run (T.unlines
        [ "(let ((x Nothing))"
        , "  (if-let (y x) (print \"yes\") (print \"no\")))"
        ]) >>= (`shouldBe` "no")

    it "if-let binds the value" $ do
      run (T.unlines
        [ "(let ((x (Just 42)))"
        , "  (if-let (y x)"
        , "    (case y ((Just v) (print (int-to-str v))))"
        , "    (print \"none\")))"
        ]) >>= (`shouldBe` "42")

    it "when-let on truthy" $ do
      run (T.unlines
        [ "(let ((x (Cons 1 Empty)))"
        , "  (when-let (y x) (print \"yes\")))"
        ]) >>= (`shouldBe` "yes")

    it "when-let on falsy" $ do
      run (T.unlines
        [ "(let ((x Empty))"
        , "  (when-let (y x) (print \"found\")))"
        , "(print \"done\")"
        ]) >>= (`shouldBe` "done")

    it "unless-let on falsy" $ do
      run (T.unlines
        [ "(let ((x Nothing))"
        , "  (unless-let (y x) (print \"empty\")))"
        ]) >>= (`shouldBe` "empty")

    it "unless-let on truthy" $ do
      run (T.unlines
        [ "(let ((x (Just 1)))"
        , "  (unless-let (y x) (print \"empty\")))"
        , "(print \"done\")"
        ]) >>= (`shouldBe` "done")

  describe "record types" $ do
    it "record field access string" $
      run (T.unlines
        [ "(type Person () (Person (name %STR) (age %INT)))"
        , "(let ((p (Person \"alice\" 30)))"
        , "  (print (.name p)))"
        ]) >>= (`shouldBe` "alice")

    it "record field access int" $
      run (T.unlines
        [ "(type Person () (Person (name %STR) (age %INT)))"
        , "(let ((p (Person \"alice\" 30)))"
        , "  (print (int-to-str (.age p))))"
        ]) >>= (`shouldBe` "30")

    it "record pattern match still works" $
      run (T.unlines
        [ "(type Point () (Point (x %INT) (y %INT)))"
        , "(let ((p (Point 3 4)))"
        , "  (case p ((Point a b) (print (int-to-str (add a b))))))"
        ]) >>= (`shouldBe` "7")

    it "record with type parameter" $
      run (T.unlines
        [ "(type Box (a) (Box (val a)))"
        , "(let ((b (Box 42)))"
        , "  (print (int-to-str (.val b))))"
        ]) >>= (`shouldBe` "42")

    it "nested field access" $
      run (T.unlines
        [ "(type Box (a) (Box (val a)))"
        , "(let ((b (Box (Box 42))))"
        , "  (print (int-to-str (.val (.val b)))))"
        ]) >>= (`shouldBe` "42")

  describe "&rest" $ do
    it "captures remaining args as list" $
      run (T.unlines
        [ "(let ((f (lam (a &rest xs) (case xs"
        , "  ((Cons h _) (print (int-to-str (add a h))))"
        , "  ((Empty) (print (int-to-str a)))))))"
        , "  (f 10 20))"
        ]) >>= (`shouldBe` "30")

    it "no extra args gives Empty" $
      run (T.unlines
        [ "(let ((f (lam (a &rest xs) (case xs"
        , "  ((Empty) (print \"empty\"))"
        , "  ((Cons _ _) (print \"notempty\"))))))"
        , "  (f 1))"
        ]) >>= (`shouldBe` "empty")

    it "captures multiple extra args" $
      run (T.unlines
        [ "(let ((go (lam ((xs %(List %INT))) (case xs"
        , "  ((Empty) 0)"
        , "  ((Cons h t) (add h (go t)))))))"
        , "  (let ((sum-all (lam (&rest xs) (go xs))))"
        , "    (print (int-to-str (sum-all 1 2 3 4)))))"
        ]) >>= (`shouldBe` "10")

    it "rest with no required params" $
      run (T.unlines
        [ "(let ((f (lam (&rest xs) (case xs"
        , "  ((Empty) (print \"none\"))"
        , "  ((Cons h _) (print (int-to-str h)))))))"
        , "  (f 42))"
        ]) >>= (`shouldBe` "42")

  describe "%opt" $ do
    it "uses default when arg omitted" $
      run (T.unlines
        [ "(let ((f (lam (a %opt (b 10)) (add a b))))"
        , "  (print (int-to-str (f 5))))"
        ]) >>= (`shouldBe` "15")

    it "uses provided arg over default" $
      run (T.unlines
        [ "(let ((f (lam (a %opt (b 10)) (add a b))))"
        , "  (print (int-to-str (f 5 20))))"
        ]) >>= (`shouldBe` "25")

    it "multiple optional params all defaulted" $
      run (T.unlines
        [ "(let ((f (lam (%opt (a 1) (b 2) (c 3)) (add a (add b c)))))"
        , "  (print (int-to-str (f))))"
        ]) >>= (`shouldBe` "6")

    it "multiple optional params partially provided" $
      run (T.unlines
        [ "(let ((f (lam (%opt (a 1) (b 2) (c 3)) (add a (add b c)))))"
        , "  (print (int-to-str (f 10))))"
        ]) >>= (`shouldBe` "15")

    it "multiple optional params all provided" $
      run (T.unlines
        [ "(let ((f (lam (%opt (a 1) (b 2) (c 3)) (add a (add b c)))))"
        , "  (print (int-to-str (f 10 20 30))))"
        ]) >>= (`shouldBe` "60")

    it "opt with string default" $
      run (T.unlines
        [ "(let ((greet (lam (%opt (name \"world\")) (concat \"hello \" name))))"
        , "  (print (greet)))"
        ]) >>= (`shouldBe` "hello world")

    it "opt with string provided" $
      run (T.unlines
        [ "(let ((greet (lam (%opt (name \"world\")) (concat \"hello \" name))))"
        , "  (print (greet \"alice\")))"
        ]) >>= (`shouldBe` "hello alice")

  describe "&key" $ do
    it "basic keyword args" $
      run (T.unlines
        [ "(let ((f (lam (&key (x 0) (y 0)) (add x y))))"
        , "  (print (int-to-str (f &key x 10 &key y 20))))"
        ]) >>= (`shouldBe` "30")

    it "keyword args reordered" $
      run (T.unlines
        [ "(let ((f (lam (&key (x 0) (y 0)) (sub x y))))"
        , "  (print (int-to-str (f &key y 3 &key x 10))))"
        ]) >>= (`shouldBe` "7")

    it "keyword args with defaults" $
      run (T.unlines
        [ "(let ((f (lam (&key (x 0) (y 100)) (add x y))))"
        , "  (print (int-to-str (f &key x 5))))"
        ]) >>= (`shouldBe` "105")

    it "all keyword args defaulted" $
      run (T.unlines
        [ "(let ((f (lam (&key (x 1) (y 2)) (add x y))))"
        , "  (print (int-to-str (f))))"
        ]) >>= (`shouldBe` "3")

    it "keyword with required params" $
      run (T.unlines
        [ "(let ((f (lam (a &key (x 0)) (add a x))))"
        , "  (print (int-to-str (f 10 &key x 5))))"
        ]) >>= (`shouldBe` "15")

  describe "typeclasses" $ do
    it "basic class + instance + method call" $
      run (T.unlines
        [ "(cls SHOW () (a) (show %a %STR))"
        , "(inst SHOW %INT (show (lam ((x %INT)) (int-to-str x))))"
        , "(print (show 42))"
        ]) >>= (`shouldBe` "42")

    it "class method with multiple args" $
      run (T.unlines
        [ "(cls EQUAL () (a) (equal %a %a %BOOL))"
        , "(inst EQUAL %INT (equal (lam ((x %INT) (y %INT)) (eqi x y))))"
        , "(print (int-to-str (if (equal 1 1) 1 0)))"
        ]) >>= (`shouldBe` "1")

    it "multiple instances of same class" $
      run (T.unlines
        [ "(cls SHOW () (a) (show %a %STR))"
        , "(inst SHOW %INT (show (lam ((x %INT)) (int-to-str x))))"
        , "(inst SHOW %STR (show (lam ((x %STR)) x)))"
        , "(print (show 42))"
        ]) >>= (`shouldBe` "42")

    it "different instances dispatched by type" $
      run (T.unlines
        [ "(cls SHOW () (a) (show %a %STR))"
        , "(inst SHOW %INT (show (lam ((x %INT)) (int-to-str x))))"
        , "(inst SHOW %STR (show (lam ((x %STR)) (concat \"[\" (concat x \"]\")))))"
        , "(print (show \"hi\"))"
        ]) >>= (`shouldBe` "[hi]")

    it "class with multiple methods" $
      run (T.unlines
        [ "(cls EQUAL () (a)"
        , "  (equal %a %a %BOOL)"
        , "  (nequal %a %a %BOOL))"
        , "(inst EQUAL %INT"
        , "  (equal (lam ((x %INT) (y %INT)) (eqi x y)))"
        , "  (nequal (lam ((x %INT) (y %INT)) (not (eqi x y)))))"
        , "(print (int-to-str (if (nequal 1 2) 1 0)))"
        ]) >>= (`shouldBe` "1")

    it "class method used in let binding" $
      run (T.unlines
        [ "(cls SHOW () (a) (show %a %STR))"
        , "(inst SHOW %INT (show (lam ((x %INT)) (int-to-str x))))"
        , "(let ((result (show 99)))"
        , "  (print result))"
        ]) >>= (`shouldBe` "99")

    it "class method in if branches" $
      run (T.unlines
        [ "(cls SHOW () (a) (show %a %STR))"
        , "(inst SHOW %INT (show (lam ((x %INT)) (int-to-str x))))"
        , "(print (if TRUE (show 1) (show 2)))"
        ]) >>= (`shouldBe` "1")

    it "instance for float type" $
      run (T.unlines
        [ "(cls SHOW () (a) (show %a %STR))"
        , "(inst SHOW %FLT (show (lam ((x %FLT)) (flt-to-str x))))"
        , "(print (show 3.14))"
        ]) >>= (`shouldBe` "3.14")

    it "method used with ADT" $
      run (T.unlines
        [ "(TYPE Color () (Red) (Green) (Blue))"
        , "(cls SHOW () (a) (show %a %STR))"
        , "(inst SHOW %Color (show (lam ((c %Color)) (case c"
        , "  ((Red) \"red\")"
        , "  ((Green) \"green\")"
        , "  ((Blue) \"blue\")))))"
        , "(print (show Red))"
        ]) >>= (`shouldBe` "red")

    it "method result passed to another function" $
      run (T.unlines
        [ "(cls SHOW () (a) (show %a %STR))"
        , "(inst SHOW %INT (show (lam ((x %INT)) (int-to-str x))))"
        , "(print (concat \"val=\" (show 7)))"
        ]) >>= (`shouldBe` "val=7")

    it "class method in nested let" $
      run (T.unlines
        [ "(cls SHOW () (a) (show %a %STR))"
        , "(inst SHOW %INT (show (lam ((x %INT)) (int-to-str x))))"
        , "(let ((a (show 1)))"
        , "  (let ((b (show 2)))"
        , "    (print (concat a b))))"
        ]) >>= (`shouldBe` "12")

    it "two different classes" $
      run (T.unlines
        [ "(cls SHOW () (a) (show %a %STR))"
        , "(cls EQUAL () (a) (equal %a %a %BOOL))"
        , "(inst SHOW %INT (show (lam ((x %INT)) (int-to-str x))))"
        , "(inst EQUAL %INT (equal (lam ((x %INT) (y %INT)) (eqi x y))))"
        , "(print (concat (show 42) (if (equal 1 1) \" yes\" \" no\")))"
        ]) >>= (`shouldBe` "42 yes")

    it "polymorphic function with class method" $
      run (T.unlines
        [ "(cls SHOW () (a) (show %a %STR))"
        , "(inst SHOW %INT (show (lam ((x %INT)) (int-to-str x))))"
        , "(let ((print-show (lam (x) (print (show x)))))"
        , "  (print-show 42))"
        ]) >>= (`shouldBe` "42")

    it "polymorphic function called multiple times same type" $
      run (T.unlines
        [ "(cls SHOW () (a) (show %a %STR))"
        , "(inst SHOW %INT (show (lam ((x %INT)) (int-to-str x))))"
        , "(let ((to-str (lam (x) (show x))))"
        , "  (let ((_ (print (to-str 1))))"
        , "    (print (to-str 2))))"
        ]) >>= (`shouldBe` "1\n2")

    it "polymorphic function called with different types" $
      run (T.unlines
        [ "(cls SHOW () (a) (show %a %STR))"
        , "(inst SHOW %INT (show (lam ((x %INT)) (int-to-str x))))"
        , "(inst SHOW %STR (show (lam ((x %STR)) (concat \"[\" (concat x \"]\")))))"
        , "(let ((display (lam (x) (show x))))"
        , "  (let ((_ (print (display 42))))"
        , "    (print (display \"hi\"))))"
        ]) >>= (`shouldBe` "42\n[hi]")

    it "polymorphic function with multi-method class" $
      run (T.unlines
        [ "(cls EQUAL () (a)"
        , "  (equal %a %a %BOOL)"
        , "  (nequal %a %a %BOOL))"
        , "(inst EQUAL %INT"
        , "  (equal (lam ((x %INT) (y %INT)) (eqi x y)))"
        , "  (nequal (lam ((x %INT) (y %INT)) (not (eqi x y)))))"
        , "(let ((check (lam (x y) (if (equal x y) (print \"eq\") (print \"neq\")))))"
        , "  (check 1 1))"
        ]) >>= (`shouldBe` "eq")

    it "polymorphic with multiple classes" $
      run (T.unlines
        [ "(cls SHOW () (a) (show %a %STR))"
        , "(cls EQUAL () (a) (equal %a %a %BOOL))"
        , "(inst SHOW %INT (show (lam ((x %INT)) (int-to-str x))))"
        , "(inst EQUAL %INT (equal (lam ((x %INT) (y %INT)) (eqi x y))))"
        , "(let ((show-eq (lam (x y)"
        , "  (if (equal x y) (show x) (concat (show x) (concat \" != \" (show y)))))))"
        , "  (print (show-eq 1 1)))"
        ]) >>= (`shouldBe` "1")

    it "polymorphic function with ADT instance" $
      run (T.unlines
        [ "(TYPE Color () (Red) (Green) (Blue))"
        , "(cls SHOW () (a) (show %a %STR))"
        , "(inst SHOW %Color (show (lam ((c %Color)) (case c"
        , "  ((Red) \"red\") ((Green) \"green\") ((Blue) \"blue\")))))"
        , "(let ((display (lam (x) (print (show x)))))"
        , "  (display Red))"
        ]) >>= (`shouldBe` "red")

  describe "parametric typeclass instances" $ do
    it "instance for Maybe a matches Maybe Int" $
      run (T.unlines
        [ "(cls BOOLISH () (a) (boolish? %a %BOOL))"
        , "(inst BOOLISH %(Maybe a)"
        , "  (boolish? (lam ((x %(Maybe a))) (case x ((Just _) true) (_ false)))))"
        , "(print (if (boolish? (Just 1)) \"yes\" \"no\"))"
        ]) >>= (`shouldBe` "yes")

    it "instance for Maybe a matches Nothing" $
      run (T.unlines
        [ "(cls BOOLISH () (a) (boolish? %a %BOOL))"
        , "(inst BOOLISH %(Maybe a)"
        , "  (boolish? (lam ((x %(Maybe a))) (case x ((Just _) true) (_ false)))))"
        , "(print (if (boolish? Nothing) \"yes\" \"no\"))"
        ]) >>= (`shouldBe` "no")

    it "instance for List a" $
      run (T.unlines
        [ "(cls BOOLISH () (a) (boolish? %a %BOOL))"
        , "(inst BOOLISH %(List a)"
        , "  (boolish? (lam ((x %(List a))) (case x ((Cons _ _) true) (_ false)))))"
        , "(print (if (boolish? (Cons 1 Empty)) \"yes\" \"no\"))"
        ]) >>= (`shouldBe` "yes")

    it "parametric and concrete instances coexist" $
      run (T.unlines
        [ "(cls SHOW () (a) (show %a %STR))"
        , "(inst SHOW %INT (show (lam ((x %INT)) (int-to-str x))))"
        , "(inst SHOW %(Maybe a)"
        , "  (show (lam ((x %(Maybe a))) (case x ((Just _) \"Just\") (_ \"Nothing\")))))"
        , "(progn"
        , "  (print (show 42))"
        , "  (print (show (Just 1))))"
        ]) >>= (`shouldBe` "42\nJust")

    it "instance for Either a b (two type params)" $
      run (T.unlines
        [ "(cls BOOLISH () (a) (boolish? %a %BOOL))"
        , "(inst BOOLISH %(Either a b)"
        , "  (boolish? (lam ((x %(Either a b))) (case x ((Right _) true) (_ false)))))"
        , "(progn"
        , "  (print (if (boolish? (Right 1)) \"yes\" \"no\"))"
        , "  (print (if (boolish? (Left 1)) \"yes\" \"no\")))"
        ]) >>= (`shouldBe` "yes\nno")

  describe "typeclass errors" $ do
    it "error on class method call with no matching instance" $
      shouldFailToCompile
        (T.unlines
          [ "(cls SHOW () (a) (show %a %STR))"
          , "(show 42)"
          ])
        "no instance"

    it "error on truthy call with no matching instance" $
      shouldFailToCompile
        "(truthy (Pair 1 2))"
        "no instance"

  describe "EQ typeclass" $ do
    it "eq int true" $
      run "(print (if (eq 1 1) \"y\" \"n\"))" >>= (`shouldBe` "y")
    it "eq int false" $
      run "(print (if (eq 1 2) \"y\" \"n\"))" >>= (`shouldBe` "n")
    it "eq flt true" $
      run "(print (if (eq 1.0 1.0) \"y\" \"n\"))" >>= (`shouldBe` "y")
    it "eq flt false" $
      run "(print (if (eq 1.0 2.0) \"y\" \"n\"))" >>= (`shouldBe` "n")
    it "eq str true" $
      run "(print (if (eq \"a\" \"a\") \"y\" \"n\"))" >>= (`shouldBe` "y")
    it "eq str false" $
      run "(print (if (eq \"a\" \"b\") \"y\" \"n\"))" >>= (`shouldBe` "n")
    it "eq bool true-true" $
      run "(print (if (eq true true) \"y\" \"n\"))" >>= (`shouldBe` "y")
    it "eq bool true-false" $
      run "(print (if (eq true false) \"y\" \"n\"))" >>= (`shouldBe` "n")
    it "eq bool false-false" $
      run "(print (if (eq false false) \"y\" \"n\"))" >>= (`shouldBe` "y")

  describe "BOOL TRUTHY instance" $ do
    it "if_ with true" $
      run "(if_ true (print \"yes\") (print \"no\"))" >>= (`shouldBe` "yes")

    it "if_ with false" $
      run "(if_ false (print \"yes\") (print \"no\"))" >>= (`shouldBe` "no")

    it "when with true" $
      run (T.unlines
        [ "(when true (print \"yes\"))"
        , "(print \"done\")"
        ]) >>= (`shouldBe` "yes\ndone")

    it "when with false" $
      run (T.unlines
        [ "(when false (print \"nope\"))"
        , "(print \"done\")"
        ]) >>= (`shouldBe` "done")

    it "unless with false" $
      run (T.unlines
        [ "(unless false (print \"ran\"))"
        , "(print \"done\")"
        ]) >>= (`shouldBe` "ran\ndone")

    it "unless with true" $
      run (T.unlines
        [ "(unless true (print \"nope\"))"
        , "(print \"done\")"
        ]) >>= (`shouldBe` "done")

  describe "tail call optimization" $ do
    it "tail-recursive countdown" $
      run (T.unlines
        [ "(let ((countdown (lam (n)"
        , "  (if (lei n 0) (print \"done\")"
        , "    (countdown (sub n 1))))))"
        , "  (countdown 1000000))"
        ]) >>= (`shouldBe` "done")

    it "tail-recursive accumulator" $
      run (T.unlines
        [ "(let ((sum-to (lam (n acc)"
        , "  (if (lei n 0) (print (int-to-str acc))"
        , "    (sum-to (sub n 1) (add acc n))))))"
        , "  (sum-to 1000000 0))"
        ]) >>= (`shouldBe` "500000500000")

    it "tail recursion with three params" $
      run (T.unlines
        [ "(let ((go (lam (n a b)"
        , "  (if (lei n 0) (print (int-to-str (add a b)))"
        , "    (go (sub n 1) (add a 1) (add b 2))))))"
        , "  (go 1000000 0 0))"
        ]) >>= (`shouldBe` "3000000")

    it "tail recursion through let body" $
      run (T.unlines
        [ "(let ((loop (lam (n acc)"
        , "  (if (lei n 0) (print (int-to-str acc))"
        , "    (let ((next (sub n 1))"
        , "          (new-acc (add acc n)))"
        , "      (loop next new-acc))))))"
        , "  (loop 1000000 0))"
        ]) >>= (`shouldBe` "500000500000")

  describe "ffi" $ do
    it "ffi :link-name targets the external symbol instead of the pllisp binding name" $ do
      ir <- pipeline (T.unlines
        [ "(ffi c-sqrt (:link-name \"sqrt\") (%FLT) %FLT)"
        , "(print \"ok\")"
        ])
      T.isInfixOf "declare double @sqrt(double)" ir `shouldBe` True
      T.isInfixOf "declare double @c_sqrt(double)" ir `shouldBe` False

    it "ffi without :link-name still targets the binding name" $ do
      ir <- pipeline (T.unlines
        [ "(ffi sqrt (%FLT) %FLT)"
        , "(print \"ok\")"
        ])
      T.isInfixOf "declare double @sqrt(double)" ir `shouldBe` True

    it "ffi c-sqrt (:link-name \"sqrt\") (double -> double)" $
      run (T.unlines
        [ "(ffi c-sqrt (:link-name \"sqrt\") (%FLT) %FLT)"
        , "(print (flt-to-str (c-sqrt 4.0)))"
        ]) >>= (`shouldBe` "2")

    it "ffi c-pow (:link-name \"pow\") (double, double -> double)" $
      run (T.unlines
        [ "(ffi c-pow (:link-name \"pow\") (%FLT %FLT) %FLT)"
        , "(print (flt-to-str (c-pow 2.0 10.0)))"
        ]) >>= (`shouldBe` "1024")

    it "ffi c-labs (:link-name \"labs\") (long -> long)" $
      run (T.unlines
        [ "(ffi c-labs (:link-name \"labs\") (%INT) %INT)"
        , "(print (int-to-str (c-labs (neg 42))))"
        ]) >>= (`shouldBe` "42")

    it "ffi c-atol (:link-name \"atol\") (string -> long)" $
      run (T.unlines
        [ "(ffi c-atol (:link-name \"atol\") (%STR) %INT)"
        , "(print (int-to-str (c-atol \"123\")))"
        ]) >>= (`shouldBe` "123")

  describe "ffi trampolines" $ do
    it "ffi c-abs (:link-name \"abs\") (i32 -> i32) trampoline" $
      run (T.unlines
        [ "(ffi c-abs (:link-name \"abs\") (%I32) %I32)"
        , "(print (int-to-str (c-abs (neg 42))))"
        ]) >>= (`shouldBe` "42")

    it "ffi c-fabsf (:link-name \"fabsf\") (f32 -> f32) trampoline" $
      run (T.unlines
        [ "(ffi c-fabsf (:link-name \"fabsf\") (%F32) %F32)"
        , "(print (flt-to-str (c-fabsf (negf 3.5))))"
        ]) >>= (`shouldBe` "3.5")

    it "ffi mixed types: i32 trampoline result usable as i64" $
      run (T.unlines
        [ "(ffi c-abs (:link-name \"abs\") (%I32) %I32)"
        , "(print (int-to-str (add (c-abs (neg 100)) 1)))"
        ]) >>= (`shouldBe` "101")

    it "ffi existing types still work without trampoline" $
      run (T.unlines
        [ "(ffi c-labs (:link-name \"labs\") (%INT) %INT)"
        , "(print (int-to-str (c-labs (neg 99))))"
        ]) >>= (`shouldBe` "99")

  describe "ffi structs" $ do
    it "allocate struct and read field" $
      run (T.unlines
        [ "(ffi-struct Point (x %I32) (y %I32))"
        , "(let ((p (Point 10 20)))"
        , "  (print (int-to-str (.x p))))"
        ]) >>= (`shouldBe` "10")

    it "read second field" $
      run (T.unlines
        [ "(ffi-struct Point (x %I32) (y %I32))"
        , "(let ((p (Point 3 7)))"
        , "  (print (int-to-str (.y p))))"
        ]) >>= (`shouldBe` "7")

    it "struct with mixed types" $
      run (T.unlines
        [ "(ffi-struct Rec (a %I64) (b %F64) (c %I32))"
        , "(let ((r (Rec 42 3.14 7)))"
        , "  (print (int-to-str (.a r))))"
        ]) >>= (`shouldBe` "42")

    it "struct float field" $
      run (T.unlines
        [ "(ffi-struct Rec (a %I64) (b %F64))"
        , "(let ((r (Rec 1 2.5)))"
        , "  (print (flt-to-str (.b r))))"
        ]) >>= (`shouldBe` "2.5")

    it "struct passed to ffi function" $
      run (T.unlines
        [ "(ffi-struct FPair (a %I32) (b %I32))"
        , "(let ((p (FPair 100 200)))"
        , "  (print (int-to-str (add (.a p) (.b p)))))"
        ]) >>= (`shouldBe` "300")

  describe "ffi variadics" $ do
    it "ffi-var :link-name targets the external symbol instead of the pllisp binding name" $ do
      ir <- pipeline (T.unlines
        [ "(ffi-var c-printf (:link-name \"printf\") (%PTR) %I32)"
        , "(print \"ok\")"
        ])
      T.isInfixOf "declare i32 @printf(ptr, ...)" ir `shouldBe` True
      T.isInfixOf "declare i32 @c_printf(ptr, ...)" ir `shouldBe` False

    it "variadic c-printf with :link-name" $
      run (T.unlines
        [ "(ffi-var c-printf (:link-name \"printf\") (%PTR) %I32)"
        , "(c-printf \"%ld\" 42)"
        ]) >>= (`shouldBe` "42")

    it "variadic c-printf one arg" $
      run (T.unlines
        [ "(ffi-var c-printf (:link-name \"printf\") (%PTR) %I32)"
        , "(c-printf \"%ld\" 42)"
        ]) >>= (`shouldBe` "42")

    it "variadic c-printf two args" $
      run (T.unlines
        [ "(ffi-var c-printf (:link-name \"printf\") (%PTR) %I32)"
        , "(c-printf \"%ld+%ld\" 10 20)"
        ]) >>= (`shouldBe` "10+20")

    it "variadic c-printf float" $
      run (T.unlines
        [ "(ffi-var c-printf (:link-name \"printf\") (%PTR) %I32)"
        , "(c-printf \"%.1f\" 3.5)"
        ]) >>= (`shouldBe` "3.5")

    it "rejects unsupported variadic fixed parameter types" $
      shouldFailToCompile
        "(ffi-var c-bad (:link-name \"bad\") ((%ARR 4 %I8)) %I32)"
        "ffi-var does not support by-value struct or array types"

    it "rejects unsupported variadic extra argument types" $
      shouldFailToCompile
        (T.unlines
          [ "(ffi-var c-printf (:link-name \"printf\") (%PTR) %I32)"
          , "(c-printf \"%p\" (Cons 1 Empty))"
          ])
        "unsupported variadic argument type"

  -- FEATURE: String marshaling (pllisp strings ARE C char*)
  describe "ffi string marshaling" $ do
    it "pass pllisp string to C strlen" $
      run (T.unlines
        [ "(ffi c-strlen (:link-name \"strlen\") (%PTR) %I64)"
        , "(print (int-to-str (c-strlen \"hello\")))"
        ]) >>= (`shouldBe` "5")

    it "pass pllisp string to C puts" $
      run (T.unlines
        [ "(ffi c-puts (:link-name \"puts\") (%PTR) %I32)"
        , "(c-puts \"world\")"
        ]) >>= (`shouldBe` "world")

    it "receive C string from snprintf into buffer" $
      run (T.unlines
        [ "(ffi-var c-snprintf (:link-name \"snprintf\") (%PTR %I64 %PTR) %I32)"
        , "(let ((buf (substr \"xxxxxxxxxxxxxxxxxxxx\" 0 20)))"
        , "  (let ((_ (c-snprintf buf 20 \"%ld\" 42)))"
        , "    (print buf)))"
        ]) >>= (`shouldBe` "42")

  -- FEATURE: Enums (named integer constants)
  describe "ffi enums" $ do
    it "enum value used as integer" $
      run (T.unlines
        [ "(ffi-enum Color (RED 0) (GREEN 1) (BLUE 2))"
        , "(print (int-to-str GREEN))"
        ]) >>= (`shouldBe` "1")

    it "enum in comparison" $
      run (T.unlines
        [ "(ffi-enum Status (OK 0) (ERR 1))"
        , "(let ((s OK))"
        , "  (print (if (eqi s OK) \"success\" \"failure\")))"
        ]) >>= (`shouldBe` "success")

    it "enum passed to function" $
      run (T.unlines
        [ "(ffi-enum Dir (UP 0) (DOWN 1) (WEST 2) (EAST 3))"
        , "(let ((describe (lam (d)"
        , "        (if (eqi d UP) \"up\""
        , "          (if (eqi d EAST) \"east\" \"other\")))))"
        , "  (print (describe EAST)))"
        ]) >>= (`shouldBe` "east")

  -- FEATURE: Arrays (fixed-size inline arrays)
  describe "ffi arrays" $ do
    it "struct with byte array, write and read" $
      run (T.unlines
        [ "(ffi-struct Buf (data (%ARR 8 %I8)) (len %I32))"
        , "(ffi-var c-snprintf (:link-name \"snprintf\") (%PTR %I64 %PTR) %I32)"
        , "(let ((b (Buf 4)))"
        , "  (let ((_ (c-snprintf (.data b) 8 \"hey\")))"
        , "    (print (.data b))))"
        ]) >>= (`shouldBe` "hey")

    it "struct with int array and scalar field" $
      run (T.unlines
        [ "(ffi-struct TaggedArr (data (%ARR 4 %I8)) (tag %I32))"
        , "(let ((p (TaggedArr 42)))"
        , "  (print (int-to-str (.tag p))))"
        ]) >>= (`shouldBe` "42")

  -- FEATURE: Nested structs (struct as field type)
  describe "ffi nested structs" $ do
    it "nested struct field access" $
      run (T.unlines
        [ "(ffi-struct Point (x %I32) (y %I32))"
        , "(ffi-struct Line (start %Point) (end %Point))"
        , "(let ((p1 (Point 1 2))"
        , "      (p2 (Point 3 4)))"
        , "  (let ((ln (Line p1 p2)))"
        , "    (print (int-to-str (.x (.start ln))))))"
        ]) >>= (`shouldBe` "1")

    it "nested struct second field" $
      run (T.unlines
        [ "(ffi-struct Point (x %I32) (y %I32))"
        , "(ffi-struct Rect (origin %Point) (size %Point))"
        , "(let ((o (Point 10 20))"
        , "      (s (Point 100 200)))"
        , "  (let ((r (Rect o s)))"
        , "    (print (int-to-str (.y (.size r))))))"
        ]) >>= (`shouldBe` "200")

  -- FEATURE: Callbacks (pllisp closure as C function pointer)
  describe "ffi callbacks" $ do
    it "wrap closure as C function pointer" $
      run (T.unlines
        [ "(ffi c-pll-test-apply-int (:link-name \"pll_test_apply_int\") (%PTR %I64) %I64)"
        , "(ffi-callback int-cb (%I64) %I64)"
        , "(let ((doubler (int-cb (lam (x) (mul x 2)))))"
        , "  (print (int-to-str (c-pll-test-apply-int doubler 21))))"
        ]) >>= (`shouldBe` "42")

    it "callback with closure capture" $
      run (T.unlines
        [ "(ffi c-pll-test-apply-int (:link-name \"pll_test_apply_int\") (%PTR %I64) %I64)"
        , "(ffi-callback int-cb (%I64) %I64)"
        , "(let ((offset 10)"
        , "      (adder (int-cb (lam (x) (add x offset)))))"
        , "  (print (int-to-str (c-pll-test-apply-int adder 32))))"
        ]) >>= (`shouldBe` "42")

    it "marshals f64 callback arguments and returns" $
      run (T.unlines
        [ "(ffi c-pll-test-apply-flt64 (:link-name \"pll_test_apply_flt64\") (%PTR %F64) %F64)"
        , "(ffi-callback flt64-cb (%F64) %F64)"
        , "(let ((twice (flt64-cb (lam (x) (mulf x 2.0)))))"
        , "  (print (flt-to-str (c-pll-test-apply-flt64 twice 21.25))))"
        ]) >>= (`shouldBe` "42.5")

    it "marshals f32 callback arguments and returns" $
      run (T.unlines
        [ "(ffi c-pll-test-apply-flt32 (:link-name \"pll_test_apply_flt32\") (%PTR %F32) %F32)"
        , "(ffi-callback flt32-cb (%F32) %F32)"
        , "(let ((bump (flt32-cb (lam (x) (addf x 1.25)))))"
        , "  (print (flt-to-str (c-pll-test-apply-flt32 bump 41.25))))"
        ]) >>= (`shouldBe` "42.5")

    it "marshals mixed numeric callback arguments" $
      run (T.unlines
        [ "(ffi c-pll-test-apply-mix-num (:link-name \"pll_test_apply_mix_num\") (%PTR %I64 %F64) %F64)"
        , "(ffi-callback mix-cb (%I64 %F64) %F64)"
        , "(let ((combine (mix-cb (lam (i x) (addf (int-to-flt i) x)))))"
        , "  (print (flt-to-str (c-pll-test-apply-mix-num combine 40 2.5))))"
        ]) >>= (`shouldBe` "42.5")

    it "rejects void callback parameters" $
      shouldFailToCompile
        "(ffi-callback bad-cb (%VOID) %I64)"
        "ffi-callback parameter types cannot be %VOID"

    it "rejects by-value callback struct parameters" $
      shouldFailToCompile
        (T.unlines
          [ "(ffi-struct Point (x %I32) (y %I32))"
          , "(ffi-callback point-cb (%Point) %I64)"
          ])
        "ffi-callback does not support by-value struct or array types"

  -- FEATURE: Struct passed to C functions via pointer
  describe "ffi struct interop" $ do
    it "pass struct to C function that reads fields" $
      run (T.unlines
        [ "(ffi-struct Point (x %I32) (y %I32))"
        , "(ffi c-pll-point-sum (:link-name \"pll_point_sum\") (%PTR) %I64)"
        , "(let ((p (Point 7 11)))"
        , "  (print (int-to-str (c-pll-point-sum p))))"
        ]) >>= (`shouldBe` "18")

    it "pass struct to C function that mutates fields" $
      run (T.unlines
        [ "(ffi-struct Point (x %I32) (y %I32))"
        , "(ffi c-pll-point-sum (:link-name \"pll_point_sum\") (%PTR) %I64)"
        , "(ffi c-pll-point-scale (:link-name \"pll_point_scale\") (%PTR %I64) %VOID)"
        , "(let ((p (Point 3 4)))"
        , "  (let ((_ (c-pll-point-scale p 10)))"
        , "    (print (int-to-str (c-pll-point-sum p)))))"
        ]) >>= (`shouldBe` "70")

  describe "ffi enum edge cases" $ do
    it "enum with large values" $
      run (T.unlines
        [ "(ffi-enum Flags (NONE 0) (READ 1) (WRITE 2) (EXEC 4) (ALL 7))"
        , "(print (int-to-str ALL))"
        ]) >>= (`shouldBe` "7")

    it "enum values used in arithmetic" $
      run (T.unlines
        [ "(ffi-enum Prio (LOW 1) (MED 5) (HIGH 10))"
        , "(print (int-to-str (add LOW HIGH)))"
        ]) >>= (`shouldBe` "11")

  describe "ffi combined features" $ do
    it "struct with array field accessed after construction" $
      run (T.unlines
        [ "(ffi-struct Msg (tag %I32) (buf (%ARR 16 %I8)))"
        , "(ffi-var c-snprintf (:link-name \"snprintf\") (%PTR %I64 %PTR) %I32)"
        , "(let ((m (Msg 42)))"
        , "  (let ((_ (c-snprintf (.buf m) 16 \"hello\")))"
        , "    (print (.buf m))))"
        ]) >>= (`shouldBe` "hello")

  describe "ffi hardening" $ do
    it "rejects duplicate names across ffi declaration forms" $
      shouldFailToCompile
        (T.unlines
          [ "(ffi c-foo (:link-name \"foo\") (%I64) %I64)"
          , "(ffi-var c-foo (:link-name \"foo\") (%PTR) %I32)"
          ])
        "duplicate ffi definition: C-FOO"

    it "rejects duplicate names across ffi struct and enum declarations" $
      shouldFailToCompile
        (T.unlines
          [ "(ffi-struct Point (x %I32))"
          , "(ffi-enum Point (ORIGIN 0))"
          ])
        "duplicate ffi definition: POINT"

    it "rejects by-value struct parameters in ffi declarations" $
      shouldFailToCompile
        (T.unlines
          [ "(ffi-struct Point (x %I32) (y %I32))"
          , "(ffi c-bad-point (:link-name \"bad_point\") (%Point) %I64)"
          ])
        "ffi does not support by-value struct or array types"

    it "struct passed to C function via polymorphic ptr" $
      run (T.unlines
        [ "(ffi-struct Point (x %I32) (y %I32))"
        , "(ffi c-pll-point-sum (:link-name \"pll_point_sum\") (%PTR) %I64)"
        , "(let ((a (Point 100 200)))"
        , "  (print (int-to-str (c-pll-point-sum a))))"
        ]) >>= (`shouldBe` "300")

  describe "mutable refs" $ do
    it "create ref and deref" $
      run "(print (int-to-str (deref (ref 42))))" >>= (`shouldBe` "42")

    it "set! changes the value" $
      run (T.unlines
        [ "(let ((r (ref 10)))"
        , "  (let ((_ (set! r 20)))"
        , "    (print (int-to-str (deref r)))))"
        ]) >>= (`shouldBe` "20")

    it "ref with string" $
      run (T.unlines
        [ "(let ((r (ref \"hello\")))"
        , "  (let ((_ (set! r \"world\")))"
        , "    (print (deref r))))"
        ]) >>= (`shouldBe` "world")

    it "ref with bool" $
      run (T.unlines
        [ "(let ((r (ref TRUE)))"
        , "  (let ((_ (set! r FALSE)))"
        , "    (print (if (deref r) \"yes\" \"no\"))))"
        ]) >>= (`shouldBe` "no")

    it "multiple set! keeps last value" $
      run (T.unlines
        [ "(let ((r (ref 1)))"
        , "  (let ((_ (set! r 2)))"
        , "    (let ((_ (set! r 3)))"
        , "      (print (int-to-str (deref r))))))"
        ]) >>= (`shouldBe` "3")

    it "ref shared through closure" $
      run (T.unlines
        [ "(let ((counter (ref 0))"
        , "      (inc (lam () (set! counter (add (deref counter) 1)))))"
        , "  (let ((_ (inc))"
        , "        (_ (inc))"
        , "        (_ (inc)))"
        , "    (print (int-to-str (deref counter)))))"
        ]) >>= (`shouldBe` "3")

    it "set! returns unit" $
      run (T.unlines
        [ "(let ((r (ref 0)))"
        , "  (let ((u (set! r 42)))"
        , "    (print (int-to-str (deref r)))))"
        ]) >>= (`shouldBe` "42")

    it "ref with float" $
      run (T.unlines
        [ "(let ((r (ref 3.14)))"
        , "  (let ((_ (set! r 2.72)))"
        , "    (print (flt-to-str (deref r)))))"
        ]) >>= (`shouldBe` "2.72")

    it "ref with ADT value" $
      run (T.unlines
        [ "(TYPE Option (a) (Some a) (None))"
        , "(let ((r (ref None)))"
        , "  (let ((_ (set! r (Some 42))))"
        , "    (CASE (deref r)"
        , "      ((Some x) (print (int-to-str x)))"
        , "      (None (print \"none\")))))"
        ]) >>= (`shouldBe` "42")

  describe "newtype erasure" $ do
    it "IR has no GC_malloc for newtype construction" $ do
      ir <- pipeline (T.unlines
        [ "(TYPE Name () (MkName %Str))"
        , "(let ((n (MkName \"alice\"))) (CASE n ((MkName s) (print s))))"
        ])
      -- Newtype (single-constructor, single-field) should be erased.
      -- Dict-passing may add GC_malloc for class instance closures,
      -- but no allocation should appear for MkName itself.
      -- Check: no GC_malloc that stores a string pointer (MkName stores %Str).
      -- Dict allocations store function pointers (@__lambda_*), not strings.
      let mainLines = dropWhile (not . T.isPrefixOf "define i32 @main") (T.lines ir)
          mainBody = T.unlines (takeWhile (not . T.isPrefixOf "}") (drop 1 mainLines))
      -- MkName would need tag + string pointer. If erased, no such pattern.
      T.isInfixOf "store ptr @str" mainBody `shouldBe` False

    it "single-ctor single-field string wrapper is zero-cost" $
      run (T.unlines
        [ "(TYPE Name () (MkName %Str))"
        , "(let ((n (MkName \"alice\")))"
        , "  (CASE n ((MkName s) (print s))))"
        ]) >>= (`shouldBe` "alice")

    it "newtype wrapping another ADT" $
      run (T.unlines
        [ "(TYPE Pair (a b) (MkPair a b))"
        , "(TYPE Wrapped () (Wrap %(Pair %Int %Int)))"
        , "(let ((w (Wrap (MkPair 3 4))))"
        , "  (CASE w ((Wrap p)"
        , "    (CASE p ((MkPair x y) (print (int-to-str (add x y))))))))"
        ]) >>= (`shouldBe` "7")

    it "multi-constructor ADT is NOT erased" $
      run (T.unlines
        [ "(TYPE Maybe (a) (Just a) (Nothing))"
        , "(let ((x (Just 42)))"
        , "  (CASE x"
        , "    ((Just v) (print (int-to-str v)))"
        , "    (Nothing (print \"none\"))))"
        ]) >>= (`shouldBe` "42")

    it "multi-field single-constructor is NOT erased" $
      run (T.unlines
        [ "(TYPE Pair2 (a b) (MkPair2 a b))"
        , "(let ((p (MkPair2 10 20)))"
        , "  (CASE p ((MkPair2 x y) (print (int-to-str (add x y))))))"
        ]) >>= (`shouldBe` "30")

    it "newtype used in let binding" $
      run (T.unlines
        [ "(TYPE Tag () (MkTag %Str))"
        , "(let ((t (MkTag \"hello\")))"
        , "  (let ((s (CASE t ((MkTag x) x))))"
        , "    (print s)))"
        ]) >>= (`shouldBe` "hello")

    it "newtype passed to function" $
      run (T.unlines
        [ "(TYPE Label () (MkLabel %Str))"
        , "(let ((show-label (lam (l)"
        , "        (CASE l ((MkLabel s) (print s))))))"
        , "  (show-label (MkLabel \"world\")))"
        ]) >>= (`shouldBe` "world")

    it "newtype in nested pattern match" $
      run (T.unlines
        [ "(TYPE Id () (MkId %Str))"
        , "(TYPE Maybe2 (a) (Just2 a) (Nothing2))"
        , "(let ((x (Just2 (MkId \"found\"))))"
        , "  (CASE x"
        , "    ((Just2 id-val) (CASE id-val ((MkId s) (print s))))"
        , "    (Nothing2 (print \"none\"))))"
        ]) >>= (`shouldBe` "found")

    it "polymorphic single-field is NOT erased" $
      run (T.unlines
        [ "(TYPE Box (a) (MkBox a))"
        , "(let ((b (MkBox (MkBox 42))))"
        , "  (CASE b ((MkBox inner)"
        , "    (CASE inner ((MkBox v) (print (int-to-str v)))))))"
        ]) >>= (`shouldBe` "42")

    it "int-wrapping single-ctor is NOT erased" $
      run (T.unlines
        [ "(TYPE Age () (MkAge %Int))"
        , "(let ((a (MkAge 25)))"
        , "  (CASE a ((MkAge n) (print (int-to-str n)))))"
        ]) >>= (`shouldBe` "25")

    it "bool-wrapping single-ctor is NOT erased" $
      run (T.unlines
        [ "(TYPE Flag () (MkFlag %Bool))"
        , "(let ((f (MkFlag true)))"
        , "  (CASE f ((MkFlag b) (if b (print \"yes\") (print \"no\")))))"
        ]) >>= (`shouldBe` "yes")

  describe "uninterned symbols" $ do
    it "usym pattern matching" $
      run (T.unlines
        [ "(let ((x :foo))"
        , "  (case x"
        , "    (:foo (print \"matched foo\"))"
        , "    (:bar (print \"matched bar\"))"
        , "    (_ (print \"other\"))))"
        ]) >>= (`shouldBe` "matched foo")

    it "usym pattern matching second arm" $
      run (T.unlines
        [ "(let ((x :bar))"
        , "  (case x"
        , "    (:foo (print \"matched foo\"))"
        , "    (:bar (print \"matched bar\"))"
        , "    (_ (print \"other\"))))"
        ]) >>= (`shouldBe` "matched bar")

    it "usym pattern matching wildcard" $
      run (T.unlines
        [ "(let ((x :baz))"
        , "  (case x"
        , "    (:foo (print \"foo\"))"
        , "    (_ (print \"other\"))))"
        ]) >>= (`shouldBe` "other")

    it "usym in let binding and case" $
      run (T.unlines
        [ "(let ((mode :verbose))"
        , "  (case mode"
        , "    (:verbose (print \"v\"))"
        , "    (:quiet (print \"q\"))"
        , "    (_ (print \"?\")))"
        , ")"
        ]) >>= (`shouldBe` "v")

    it "usym passed as function argument" $
      run (T.unlines
        [ "(let ((check (lam ((s %USYM)) %STR"
        , "  (case s"
        , "    (:verbose \"yes\")"
        , "    (_ \"no\")))))"
        , "  (print (check :verbose)))"
        ]) >>= (`shouldBe` "yes")

    it "usym function returns different results" $
      run (T.unlines
        [ "(let ((check (lam ((s %USYM)) %STR"
        , "  (case s"
        , "    (:verbose \"yes\")"
        , "    (_ \"no\")))))"
        , "  (print (check :quiet)))"
        ]) >>= (`shouldBe` "no")

    it "multiple usym let bindings" $
      run (T.unlines
        [ "(let ((a :foo) (b :bar))"
        , "  (case a"
        , "    (:foo (case b"
        , "      (:bar (print \"both\"))"
        , "      (_ (print \"a only\"))))"
        , "    (_ (print \"neither\"))))"
        ]) >>= (`shouldBe` "both")

    it "usym returned from function" $
      run (T.unlines
        [ "(let ((get-mode (lam () %USYM :verbose)))"
        , "  (case (get-mode)"
        , "    (:verbose (print \"v\"))"
        , "    (_ (print \"other\"))))"
        ]) >>= (`shouldBe` "v")

    it "usym in ADT field" $
      run (T.unlines
        [ "(type Tagged () (Tagged %USYM %INT))"
        , "(let ((t (Tagged :foo 42)))"
        , "  (case t ((Tagged sym n)"
        , "    (case sym"
        , "      (:foo (print (int-to-str n)))"
        , "      (_ (print \"other\"))))))"
        ]) >>= (`shouldBe` "42")

    it "usym in &rest args" $
      run (T.unlines
        [ "(let ((first-sym (lam (&rest xs) (case xs"
        , "  ((Cons h _) (case h"
        , "    (:foo (print \"got foo\"))"
        , "    (_ (print \"other\"))))"
        , "  ((Empty) (print \"none\"))))))"
        , "  (first-sym :foo :bar))"
        ]) >>= (`shouldBe` "got foo")

    it "usym type error: cannot use where INT expected" $
      shouldFailToCompile
        "(let ((x :foo)) (add x 1))"
        "cannot unify"

    it "usym type error: cannot pass INT as USYM" $
      shouldFailToCompile
        (T.unlines
          [ "(let ((f (lam ((s %USYM)) s)))"
          , "  (f 42))"
          ])
        "cannot unify"

    it "usym case without wildcard compiles" $
      run (T.unlines
        [ "(let ((x :foo))"
        , "  (case x"
        , "    (:foo (print \"yes\"))))"
        ]) >>= (`shouldBe` "yes")

    it "usym-to-str converts usym to string" $
      run "(print (usym-to-str :hello))" >>= (`shouldBe` "HELLO")

    it "str-to-usym converts string to usym" $
      run (T.unlines
        [ "(let ((s (str-to-usym \"FOO\")))"
        , "  (case s"
        , "    (:FOO (print \"matched\"))"
        , "    (_ (print \"nope\"))))"
        ]) >>= (`shouldBe` "matched")

    it "usym-to-str roundtrips with str-to-usym" $
      run (T.unlines
        [ "(let ((s (usym-to-str :test)))"
        , "  (case (str-to-usym s)"
        , "    (:TEST (print \"ok\"))"
        , "    (_ (print \"fail\"))))"
        ]) >>= (`shouldBe` "ok")

-- HELPERS

pipeline :: T.Text -> IO T.Text
pipeline = multiModulePipeline []

run :: T.Text -> IO String
run = runWith []

runWith :: [String] -> T.Text -> IO String
runWith = runWithStdin ""

runWithInput :: String -> T.Text -> IO String
runWithInput stdin' = runWithStdin stdin' []

runWithStdin :: String -> [String] -> T.Text -> IO String
runWithStdin stdin' extraArgs src = do
  ir <- pipeline src
  T.IO.writeFile "/tmp/pllisp_test.ll" ir
  T.IO.writeFile "/tmp/pll_ffi_bridge.c" Ty.ffiBridgeC
  (ec1, _, err1) <- readProcessWithExitCode
    "clang" ["/tmp/pllisp_test.ll", "/tmp/pll_ffi_bridge.c",
             "-o", "/tmp/pllisp_test_exe", "-lm", "-lpcre2-8", "-lgc", "-lffi"] ""
  case ec1 of
    ExitFailure _ -> error ("clang failed:\n" ++ err1 ++ "\nIR:\n" ++ T.unpack ir)
    ExitSuccess -> do
      (ec2, out, err2) <- readProcessWithExitCode
        "/tmp/pllisp_test_exe" extraArgs stdin'
      case ec2 of
        ExitSuccess   -> pure (strip out)
        ExitFailure c -> error ("Program exited with " ++ show c ++ ":\n" ++ err2)
  where
    strip = reverse . dropWhile (== '\n') . reverse

-- | Compile a module + main source and run the result.
-- modName: uppercase module name, modSrc: module body, unquals: unqualified imports, mainSrc: main body
runWithModule :: CST.Symbol -> T.Text -> [CST.Symbol] -> T.Text -> IO String
runWithModule modName modSrc unquals mainSrc = do
  ir <- importPipeline modName modSrc unquals mainSrc
  T.IO.writeFile "/tmp/pllisp_test.ll" ir
  T.IO.writeFile "/tmp/pll_ffi_bridge.c" Ty.ffiBridgeC
  (ec1, _, err1) <- readProcessWithExitCode
    "clang" ["/tmp/pllisp_test.ll", "/tmp/pll_ffi_bridge.c",
             "-o", "/tmp/pllisp_test_exe", "-lm", "-lpcre2-8", "-lgc", "-lffi"] ""
  case ec1 of
    ExitFailure _ -> error ("clang failed:\n" ++ err1 ++ "\nIR:\n" ++ T.unpack ir)
    ExitSuccess -> do
      (ec2, out, err2) <- readProcessWithExitCode "/tmp/pllisp_test_exe" [] ""
      case ec2 of
        ExitSuccess   -> pure (reverse . dropWhile (== '\n') . reverse $ out)
        ExitFailure c -> error ("Program exited with " ++ show c ++ ":\n" ++ err2)

importPipeline :: CST.Symbol -> T.Text -> [CST.Symbol] -> T.Text -> IO T.Text
importPipeline modName modSrc unquals mainSrc =
  let importForm = if null unquals
        then "(import " <> modName <> ")\n"
        else "(import " <> modName <> " (" <> T.intercalate " " unquals <> "))\n"
  in multiModulePipeline [(modName, modSrc)] (importForm <> mainSrc)

runWithModules :: [(CST.Symbol, T.Text)] -> T.Text -> IO String
runWithModules modules mainSrc = do
  ir <- multiModulePipeline modules mainSrc
  T.IO.writeFile "/tmp/pllisp_test.ll" ir
  T.IO.writeFile "/tmp/pll_ffi_bridge.c" Ty.ffiBridgeC
  (ec1, _, err1) <- readProcessWithExitCode
    "clang" ["/tmp/pllisp_test.ll", "/tmp/pll_ffi_bridge.c",
             "-o", "/tmp/pllisp_test_exe", "-lm", "-lpcre2-8", "-lgc", "-lffi"] ""
  case ec1 of
    ExitFailure _ -> error ("clang failed:\n" ++ err1 ++ "\nIR:\n" ++ T.unpack ir)
    ExitSuccess -> do
      (ec2, out, err2) <- readProcessWithExitCode "/tmp/pllisp_test_exe" [] ""
      case ec2 of
        ExitSuccess   -> pure (reverse . dropWhile (== '\n') . reverse $ out)
        ExitFailure c -> error ("Program exited with " ++ show c ++ ":\n" ++ err2)

runWithCLIArgs :: [String] -> T.Text -> IO String
runWithCLIArgs extraArgs mainSrc = do
  (ec2, out, err2) <- runWithCLIArgsRaw extraArgs mainSrc
  case ec2 of
    ExitSuccess   -> pure out
    ExitFailure c -> error ("Program exited with " ++ show c ++ ":\n" ++ err2)

runWithCLIArgsRaw :: [String] -> T.Text -> IO (ExitCode, String, String)
runWithCLIArgsRaw extraArgs mainSrc = do
  cliSrc <- T.IO.readFile "stdlib/CLI.pll"
  ir <- multiModulePipeline [("CLI", cliSrc)] mainSrc
  T.IO.writeFile "/tmp/pllisp_test.ll" ir
  T.IO.writeFile "/tmp/pll_ffi_bridge.c" Ty.ffiBridgeC
  (ec1, _, err1) <- readProcessWithExitCode
    "clang" ["/tmp/pllisp_test.ll", "/tmp/pll_ffi_bridge.c",
             "-o", "/tmp/pllisp_test_exe", "-lm", "-lpcre2-8", "-lgc", "-lffi"] ""
  case ec1 of
    ExitFailure _ -> error ("clang failed:\n" ++ err1 ++ "\nIR:\n" ++ T.unpack ir)
    ExitSuccess -> do
      (ec2, out, err2) <- readProcessWithExitCode "/tmp/pllisp_test_exe" extraArgs ""
      pure (ec2, reverse . dropWhile (== '\n') . reverse $ out, err2)

multiModulePipeline :: [(CST.Symbol, T.Text)] -> T.Text -> IO T.Text
multiModulePipeline modules mainSrc = do
  preludeSexprs <- Stdlib.loadPrelude
  let parsedModules = [(name, parseMod name src) | (name, src) <- modules]
      allModules = ("PRELUDE", preludeSexprs) : parsedModules
      modMap = M.fromList allModules
      rawDepMap = M.map (map CST.impModule . SExpr.preScanImports) modMap
      depMap = M.mapWithKey (\k ds ->
        if k == "PRELUDE" || "PRELUDE" `elem` ds then ds
        else "PRELUDE" : ds) rawDepMap
  order <- case Mod.dependencyOrder depMap of
    Left e  -> error ("dep order: " ++ e)
    Right o -> pure o
  (expandedMap, compileStates) <- case expandAllModules modMap order of
    Left e -> error ("module macro: " ++ e)
    Right r -> pure r
  let (finalExports, finalTyped, finalEnvs) =
        foldl (compileOneMod expandedMap compileStates modMap) (M.empty, [], TC.emptyTCEnvs) order
      mainSexprs = parseMod "MAIN" mainSrc
      mainImports = CST.Import "PRELUDE" "PRELUDE" [] : SExpr.preScanImports mainSexprs
      mainBaseState = case buildCompileBase False mainImports compileStates of
        Left e -> error ("main compile base: " ++ e)
        Right st -> st
      mainExpanded = case MacroExpand.expandModuleWith "MAIN" mainBaseState mainSexprs of
        Left e  -> error ("main macro: " ++ e)
        Right r -> MacroExpand.mrExpanded r
      mainProg = case SExpr.toProgram mainExpanded of
        Left e  -> error ("main sexpr: " ++ SExpr.ceMsg e)
        Right p -> p
      preludeExports = M.findWithDefault M.empty "PRELUDE" finalExports
      preludeImport = CST.Import "PRELUDE" "PRELUDE" (M.keys preludeExports)
      allMainImports = preludeImport : CST.progImports mainProg
      (rScope, tcCtx, nMap) = Mod.buildImportScope finalExports allMainImports
  mainExprs <- case Mod.desugarTopLevel (CST.progExprs mainProg) of
    Left e  -> error ("main desugar: " ++ e)
    Right e -> pure e
  case Resolve.resolveWith rScope nMap mainExprs of
    Left e  -> error ("main resolve: " ++ show e)
    Right resolved -> case TC.typecheckWith finalEnvs tcCtx resolved of
      Left e  -> error ("main typecheck: " ++ show e)
      Right (typed, _) ->
        let merged = Mod.mergeImportedCode finalTyped typed
        in pure $ Codegen.codegen (LL.lambdaLift (CC.closureConvert merged))
  where
    parseMod name src = case Parser.parseSExprs "<mod>" src of
      Left e  -> error ("parse " ++ T.unpack name ++ ": " ++ show e)
      Right s -> s

    buildCompileBase isPrelude imports compileStates
      | isPrelude = Right MacroExpand.primitiveState
      | otherwise = do
          states <- mapM lookupState (map CST.impModule imports)
          MacroExpand.mergeCompileStates states
      where
        lookupState name = case M.lookup name compileStates of
          Just st -> Right st
          Nothing -> Left ("missing compile-time state for " ++ T.unpack name)

    expandAllModules modMap =
      foldM expandOne (M.empty, M.empty)
      where
        expandOne (expandedMap, compileStates) modName =
          let sexprs = modMap M.! modName
              isPrelude = modName == "PRELUDE"
              imports = if isPrelude then SExpr.preScanImports sexprs
                        else CST.Import "PRELUDE" "PRELUDE" [] : SExpr.preScanImports sexprs
          in case buildCompileBase isPrelude imports compileStates of
               Left e -> Left e
               Right baseState ->
                 case MacroExpand.expandModuleWith modName baseState sexprs of
                   Left e -> Left ("macro " ++ T.unpack modName ++ ": " ++ e)
                   Right result ->
                     case MacroExpand.finalizeModuleState modName (MacroExpand.mrState result) (MacroExpand.mrExpanded result) of
                       Left e -> Left ("runtime surface " ++ T.unpack modName ++ ": " ++ e)
                       Right finalized ->
                         Right ( M.insert modName (MacroExpand.mrExpanded result) expandedMap
                               , M.insert modName finalized compileStates
                               )

    compileOneMod expandedMap compileStates _modMap (accExports, accTyped, accEnvs) modName =
      let isPrelude = modName == "PRELUDE"
          expanded = expandedMap M.! modName
          modProg = case SExpr.toProgram expanded of
            Left e  -> error ("sexpr " ++ T.unpack modName ++ ": " ++ SExpr.ceMsg e)
            Right p -> p
          preludeExports = M.findWithDefault M.empty "PRELUDE" accExports
          preludeMacroNames = case M.lookup "PRELUDE" compileStates of
            Just st -> M.keysSet (MacroExpand.csMacros st)
            Nothing -> S.empty
          cstImports = CST.progImports modProg
          allImports = if isPrelude then cstImports
                       else CST.Import "PRELUDE" "PRELUDE" (M.keys preludeExports) : cstImports
          protectedNames = if isPrelude then S.empty else M.keysSet preludeExports `S.union` preludeMacroNames
          validated = case Mod.validateProgramNames protectedNames (CST.progExprs modProg) of
            Left e -> error ("validate " ++ T.unpack modName ++ ": " ++ e)
            Right () -> ()
          collisionsChecked = case Mod.checkImportCollisions accExports allImports of
            Left e -> error ("imports " ++ T.unpack modName ++ ": " ++ e)
            Right () -> ()
          (rScope, tcCtx, nMap) = validated `seq` collisionsChecked `seq` Mod.buildImportScope accExports allImports
          exprs = case Mod.desugarTopLevel (CST.progExprs modProg) of
            Left e  -> error ("desugar " ++ T.unpack modName ++ ": " ++ e)
            Right e -> e
          (typed, modEnvs) = case Resolve.resolveWith rScope nMap exprs of
            Left e  -> error ("resolve " ++ T.unpack modName ++ ": " ++ show e)
            Right resolved -> case TC.typecheckWith accEnvs tcCtx resolved of
              Left e  -> error ("tc " ++ T.unpack modName ++ ": " ++ show e)
              Right r -> r
          modExports = Mod.collectExports modEnvs typed
      in (M.insert modName modExports accExports,
          accTyped ++ [typed],
          modEnvs)

shouldFailToCompile :: T.Text -> String -> IO ()
shouldFailToCompile src msg = do
  result <- try (pipeline src >>= evaluate) :: IO (Either ErrorCall T.Text)
  case result of
    Left (ErrorCall e)
      | msg `isInfixOf` e -> pure ()
      | otherwise -> expectationFailure
          ("expected error containing " ++ show msg ++ " but got:\n" ++ e)
    Right _ -> expectationFailure
      ("expected compilation to fail with: " ++ msg ++ " but it succeeded")

shouldFailToCompileModules :: [(CST.Symbol, T.Text)] -> T.Text -> String -> IO ()
shouldFailToCompileModules modules src msg = do
  result <- try (multiModulePipeline modules src >>= evaluate) :: IO (Either ErrorCall T.Text)
  case result of
    Left (ErrorCall e)
      | msg `isInfixOf` e -> pure ()
      | otherwise -> expectationFailure
          ("expected error containing " ++ show msg ++ " but got:\n" ++ e)
    Right _ -> expectationFailure
      ("expected compilation to fail with: " ++ msg ++ " but it succeeded")
