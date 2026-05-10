{-# LANGUAGE OverloadedStrings #-}

module ReplSpec (spec) where

import Test.Hspec

import Control.Exception (bracket, finally)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import System.Directory
  ( createDirectoryIfMissing
  , getTemporaryDirectory
  , removePathForcibly
  )
import System.FilePath ((</>))

import qualified Pllisp.Repl as Repl

spec :: Spec
spec = do
  describe "single round" $ do
    it "evaluates simple expression" $
      runRepl ["(print (int-to-str (add 1 2)))"] >>= (`shouldBe` "3")

    it "evaluates string expression" $
      runRepl ["(print \"hello\")"] >>= (`shouldBe` "hello")

    it "evaluates let binding" $
      runRepl ["(let ((x 42)) (print (int-to-str x)))"] >>= (`shouldBe` "42")

  describe "multi round" $ do
    it "top-level def persists to next round" $
      runRepl
        [ "(let ((x 42)) x)"
        , "(print (int-to-str x))"
        ] >>= (`shouldBe` "42")

    it "function def persists to next round" $
      runRepl
        [ "(let ((double (lam (n) (mul n 2)))) double)"
        , "(print (int-to-str (double 21)))"
        ] >>= (`shouldBe` "42")

    it "type def persists to next round" $
      runRepl
        [ "(TYPE Pair (a b) (MkPair a b))"
        , "(let ((p (MkPair 3 4))) (print (int-to-str (CASE p ((MkPair x y) (add x y))))))"
        ] >>= (`shouldBe` "7")

    it "ref mutation persists across rounds" $
      runRepl
        [ "(let ((counter (ref 0))) counter)"
        , "(set! counter (add (deref counter) 1))"
        , "(set! counter (add (deref counter) 1))"
        , "(print (int-to-str (deref counter)))"
        ] >>= (`shouldBe` "2")

    it "multiple defs persist across rounds" $
      runRepl
        [ "(let ((a 10)) a)"
        , "(let ((b 20)) b)"
        , "(print (int-to-str (add a b)))"
        ] >>= (`shouldBe` "30")

    it "string def persists to next round" $
      runRepl
        [ "(let ((greeting \"hello world\")) greeting)"
        , "(print greeting)"
        ] >>= (`shouldBe` "hello world")

    it "macro persists to next round" $
      runRepl
        [ "(mac double (x) `(add ,x ,x))"
        , "(print (int-to-str (double 21)))"
        ] >>= (`shouldBe` "42")

    it "compile-time helper bindings persist across rounds" $
      runRepl
        [ T.unlines
            [ "(eval-when (:compile-toplevel)"
            , "  (let ((emit-double (lam (x) `(add ,x ,x))))"
            , "    emit-double))"
            ]
        , "(mac double (x) (emit-double x))"
        , "(print (int-to-str (double 21)))"
        ] >>= (`shouldBe` "42")

    it "prelude constructors persist across rounds" $
      runRepl
        [ "(let ((xs (Cons 1 (Cons 2 Empty)))) xs)"
        , "(case xs ((Cons h _) (print (int-to-str h))))"
        ] >>= (`shouldBe` "1")

    it "pure runtime helpers persist across rounds for later compile-time code" $
      runRepl
        [ "(let ((double-int (lam ((x %INT)) (add x x)))) double-int)"
        , T.unlines
            [ "(eval-when (:compile-toplevel)"
            , "  (fun emit-double ((x %SYNTAX)) %SYNTAX"
            , "    (syntax-int (double-int (syntax-int-value x)))))"
            ]
        , "(mac doubled (x) (emit-double x))"
        , "(print (int-to-str (doubled 21)))"
        ] >>= (`shouldBe` "42")

    it "prelude regex helpers persist across rounds" $
      runRepl
        [ "(let ((digits (rx-compile \"[0-9]+\"))) digits)"
        , "(print (rx-find digits \"abc123def\"))"
        ] >>= (`shouldBe` "123")

    it "regex helpers persist across rounds for later compile-time code" $
      runRepl
        [ "(let ((digits-rx (rx-compile \"[0-9]+\"))) digits-rx)"
        , T.unlines
            [ "(eval-when (:compile-toplevel)"
            , "  (fun emit-match () %SYNTAX"
            , "    (syntax-string (rx-find digits-rx \"abc123def\"))))"
            ]
        , "(mac matched () (emit-match))"
        , "(print (matched))"
        ] >>= (`shouldBe` "123")

    it "closure captures prior-round value" $
      runRepl
        [ "(let ((base 100)) base)"
        , "(let ((add-base (lam (x) (add base x)))) add-base)"
        , "(print (int-to-str (add-base 42)))"
        ] >>= (`shouldBe` "142")

  describe "imports" $ do
    it "imports runtime state once and persists mutations across rounds" $
      withModuleProject
        [ ("STATE.pll", T.unlines
            [ "(module STATE)"
            , "(val banner \"ready\")"
            , "(var counter 41)"
            ])
        ]
        $ \cfg -> do
          sess <- Repl.newSession cfg
          flip finally (Repl.closeSession sess) $ do
            _ <- expectExec $ Repl.submitForms sess "(import STATE (banner counter))"
            _ <- expectExec $ Repl.submitForms sess "(print banner)"
            _ <- expectExec $ Repl.submitForms sess "(set! counter (add (deref counter) 1))"
            out <- expectExec $ Repl.submitForms sess "(print (int-to-str (deref counter)))"
            T.strip (Repl.reStdout out) `shouldBe` "42"

    it "imports macros for use in the same round" $
      withModuleProject
        [ ("MACROS.pll", T.unlines
            [ "(module MACROS)"
            , "(mac double (x) `(add ,x ,x))"
            ])
        ]
        $ \cfg -> do
          sess <- Repl.newSession cfg
          flip finally (Repl.closeSession sess) $ do
            out <- expectExec $ Repl.submitForms sess (T.unlines
              [ "(import MACROS)"
              , "(print (int-to-str (double 21)))"
              ])
            T.strip (Repl.reStdout out) `shouldBe` "42"

  describe "tools" $ do
    it "loads and reloads files" $
      withScratchFile "repl-load" "script.pllisp" "(let ((x 41)) x)\n" $ \cfg fp -> do
        sess <- Repl.newSession cfg
        flip finally (Repl.closeSession sess) $ do
          _ <- expectExec $ Repl.loadFile sess fp
          out1 <- expectExec $ Repl.submitForms sess "(print (int-to-str x))"
          T.strip (Repl.reStdout out1) `shouldBe` "41"
          T.IO.writeFile fp "(let ((x 99)) x)\n"
          _ <- expectExec $ Repl.reloadSession sess
          out2 <- expectExec $ Repl.submitForms sess "(print (int-to-str x))"
          T.strip (Repl.reStdout out2) `shouldBe` "99"

    it "reports the type of an expression" $
      withSession $ \sess -> do
        _ <- expectExec $ Repl.submitForms sess "(let ((x (Just 1))) x)"
        info <- expectType $ Repl.typeOf sess "x"
        Repl.rtiRendered info `shouldBe` "%(MAYBE %INT)"

    it "macroexpands with current session macros" $
      withSession $ \sess -> do
        _ <- expectExec $ Repl.submitForms sess "(mac double (x) `(add ,x ,x))"
        info <- expectExpand $ Repl.macroExpand sess "(double 21)"
        Repl.rmiRendered info `shouldBe` "(ADD 21 21)"

    it "keeps sessions isolated" $ do
      sessA <- Repl.newSession Repl.defaultConfig
      sessB <- Repl.newSession Repl.defaultConfig
      flip finally (Repl.closeSession sessA >> Repl.closeSession sessB) $ do
        _ <- expectExec $ Repl.submitForms sessA "(let ((x 1)) x)"
        _ <- expectExec $ Repl.submitForms sessB "(let ((x 2)) x)"
        outA <- expectExec $ Repl.submitForms sessA "(print (int-to-str x))"
        outB <- expectExec $ Repl.submitForms sessB "(print (int-to-str x))"
        T.strip (Repl.reStdout outA) `shouldBe` "1"
        T.strip (Repl.reStdout outB) `shouldBe` "2"

runRepl :: [T.Text] -> IO String
runRepl rounds =
  withSession $ \sess -> go sess rounds []
  where
    go _ [] acc = pure (strip (T.unpack (T.concat (reverse acc))))
    go sess (src:rest) acc = do
      res <- expectExec $ Repl.submitForms sess src
      go sess rest (Repl.reStdout res : acc)

    strip = reverse . dropWhile (== '\n') . reverse

withSession :: (Repl.ReplSession -> IO a) -> IO a
withSession action = do
  sess <- Repl.newSession Repl.defaultConfig
  action sess `finally` Repl.closeSession sess

withModuleProject :: [(FilePath, T.Text)] -> (Repl.ReplConfig -> IO a) -> IO a
withModuleProject files action = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "pllisp-repl-spec-modules"
  bracket (prepareDir dir) removePathForcibly $ \workDir -> do
    mapM_ (\(name, src) -> T.IO.writeFile (workDir </> name) src) files
    action Repl.defaultConfig { Repl.rcWorkDir = Just workDir }
  where
    prepareDir dir = do
      createDirectoryIfMissing True dir
      pure dir

withScratchFile :: String -> FilePath -> T.Text -> (Repl.ReplConfig -> FilePath -> IO a) -> IO a
withScratchFile label name src action = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> label
      fp = dir </> name
  bracket (prepareDir dir fp) removePathForcibly $ \_ ->
    action Repl.defaultConfig { Repl.rcWorkDir = Just dir } fp
  where
    prepareDir dir fp = do
      createDirectoryIfMissing True dir
      T.IO.writeFile fp src
      pure dir

expectExec :: IO (Either Repl.ReplError Repl.ReplExecResult) -> IO Repl.ReplExecResult
expectExec action = do
  result <- action
  case result of
    Left err -> expectationFailure (T.unpack (Repl.reMessage err)) >> error "unreachable"
    Right ok -> pure ok

expectType :: IO (Either Repl.ReplError Repl.ReplTypeInfo) -> IO Repl.ReplTypeInfo
expectType action = do
  result <- action
  case result of
    Left err -> expectationFailure (T.unpack (Repl.reMessage err)) >> error "unreachable"
    Right ok -> pure ok

expectExpand :: IO (Either Repl.ReplError Repl.ReplMacroInfo) -> IO Repl.ReplMacroInfo
expectExpand action = do
  result <- action
  case result of
    Left err -> expectationFailure (T.unpack (Repl.reMessage err)) >> error "unreachable"
    Right ok -> pure ok
