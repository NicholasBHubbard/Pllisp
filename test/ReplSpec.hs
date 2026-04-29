{-# LANGUAGE OverloadedStrings #-}

module ReplSpec (spec) where

import Test.Hspec

import Control.Exception (ErrorCall(..))
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import qualified Data.Text       as T
import qualified Data.Text.IO    as T.IO
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

import qualified Pllisp.Codegen        as Codegen
import qualified Pllisp.ClosureConvert as CC
import qualified Pllisp.Type           as Ty
import qualified Pllisp.CST            as CST
import qualified Pllisp.LambdaLift     as LL
import qualified Pllisp.MacroExpand    as MacroExpand
import qualified Pllisp.Parser         as Parser
import qualified Pllisp.SExpr          as SExpr
import qualified Pllisp.SrcLoc         as Loc
import qualified Pllisp.Stdlib         as Stdlib
import qualified Pllisp.Resolve        as Resolve
import qualified Pllisp.TypeCheck      as TC
import qualified Pllisp.Module         as Mod

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

    it "reports unsupported prior-round runtime helpers explicitly" $
      runRepl
        [ "(let ((printer (lam ((x %STR)) (print x)))) printer)"
        , T.unlines
            [ "(eval-when (:compile-toplevel)"
            , "  (fun emit-bad () %SYNTAX"
            , "    (printer \"hello\")))"
            ]
        , "(mac bad () (emit-bad))"
        , "(bad)"
        ] `shouldThrow` (\(ErrorCall msg) -> "PRINTER is not available at compile time" `T.isInfixOf` T.pack msg)

    it "closure captures prior-round value" $
      runRepl
        [ "(let ((base 100)) base)"
        , "(let ((add-base (lam (x) (add base x)))) add-base)"
        , "(print (int-to-str (add-base 42)))"
        ] >>= (`shouldBe` "142")

-- HELPERS

data ReplState = ReplState
  { rsScope    :: S.Set CST.Symbol
  , rsContext  :: TC.Context
  , rsGlobals  :: [(CST.Symbol, Ty.Type)]
  , rsImportedMeta :: [LL.LLExpr]
  , rsEnvs     :: TC.TCEnvs
  , rsCtState  :: MacroExpand.CompileState
  , rsPrevExprs :: [CST.Expr]  -- accumulated defs (TYPE, FFI, etc.) to replay
  }

-- | Run a sequence of REPL rounds, return the combined stdout output.
runRepl :: [T.Text] -> IO String
runRepl rounds = do
  preludeSexprs <- Stdlib.loadPrelude
  (preludeSo, preludeState) <- compilePreludeRound preludeSexprs
  stRef <- newIORef preludeState
  soFiles <- mapM (\(i, src) -> compileRound stRef i src) (zip [1 :: Int ..] rounds)
  let allSoFiles = preludeSo : soFiles
  let driverSrc = genDriver allSoFiles
  writeFile "/tmp/pll_repl_driver.c" driverSrc
  (ec1, _, err1) <- readProcessWithExitCode "clang"
    ["/tmp/pll_repl_driver.c", "-o", "/tmp/pll_repl_driver", "-ldl", "-lgc"] ""
  case ec1 of
    ExitFailure _ -> error ("clang driver failed:\n" ++ err1)
    ExitSuccess -> do
      (ec2, out, err2) <- readProcessWithExitCode "/tmp/pll_repl_driver" [] ""
      case ec2 of
        ExitSuccess   -> pure (strip out)
        ExitFailure c -> error ("REPL driver exited with " ++ show c ++ ":\n" ++ err2)
  where
    strip = reverse . dropWhile (== '\n') . reverse

compileRound :: IORef ReplState -> Int -> T.Text -> IO FilePath
compileRound stRef roundNum src = do
  st <- readIORef stRef
  let sexprs = case Parser.parseSExprs "<repl>" src of
        Left e -> error ("parse error: " ++ show e)
        Right s -> s
      expandedResult = case MacroExpand.expandModuleWith "REPL" (rsCtState st) sexprs of
        Left e -> error ("macro error: " ++ e)
        Right r -> r
      expanded = MacroExpand.mrExpanded expandedResult
      prog = case SExpr.toProgram expanded of
        Left e -> error ("sexpr error: " ++ SExpr.ceMsg e)
        Right p -> p
  (soFile, st') <- compileExprs st roundNum (rsPrevExprs st ++ CST.progExprs prog)
  let newDefs = filter isDefExpr (CST.progExprs prog)
  writeIORef stRef st'
    { rsCtState = MacroExpand.mrState expandedResult
    , rsPrevExprs = rsPrevExprs st ++ newDefs
    }
  pure soFile

compilePreludeRound :: [SExpr.SExpr] -> IO (FilePath, ReplState)
compilePreludeRound preludeSexprs = do
  let expandedResult = case MacroExpand.expandModuleWith "PRELUDE" MacroExpand.primitiveState preludeSexprs of
        Left err -> error ("prelude macro error: " ++ err)
        Right r -> r
      preludeState = case MacroExpand.finalizeModuleState "PRELUDE" (MacroExpand.mrState expandedResult) (MacroExpand.mrExpanded expandedResult) of
        Left err -> error ("prelude runtime surface error: " ++ err)
        Right st -> st
      expanded = MacroExpand.mrExpanded expandedResult
      prog = case SExpr.toProgram expanded of
        Left err -> error ("prelude sexpr error: " ++ SExpr.ceMsg err)
        Right parsed -> parsed
      initialState = ReplState S.empty M.empty [] [] TC.emptyTCEnvs preludeState []
  exprs <- case Mod.desugarTopLevel (CST.progExprs prog) of
    Left err -> error ("prelude desugar error: " ++ err)
    Right desugared -> pure desugared
  case Resolve.resolve S.empty exprs of
    Left errs -> error ("prelude resolve error: " ++ show errs)
    Right resolved -> case TC.typecheckWith TC.emptyTCEnvs M.empty resolved of
      Left errs -> error ("prelude typecheck error: " ++ show errs)
      Right (typed, envs) -> do
        let llProg = LL.lambdaLift (CC.closureConvert typed)
            ir = Codegen.codegenRepl 0 [] [] llProg
            soFile = "/tmp/pll_repl_0.so"
            llFile = "/tmp/pll_repl_0.ll"
            bridgeFile = "/tmp/pll_repl_bridge.c"
            exports = Mod.collectExports envs typed
            globals = Codegen.collectReplGlobals (LL.llExprs llProg)
            state = initialState
              { rsScope = M.keysSet exports
              , rsContext = exports
              , rsGlobals = globals
              , rsImportedMeta = LL.llExprs llProg
              , rsEnvs = envs
              }
        T.IO.writeFile llFile ir
        T.IO.writeFile bridgeFile Ty.ffiBridgeC
        (ec, _, err') <- readProcessWithExitCode "clang"
          [llFile, bridgeFile, "-shared", "-fPIC", "-o", soFile,
           "-lm", "-lpcre2-8", "-lgc", "-lffi"] ""
        case ec of
          ExitFailure _ -> error ("clang prelude .so failed:\n" ++ err' ++ "\nIR:\n" ++ T.unpack ir)
          ExitSuccess -> pure (soFile, state)

compileExprs :: ReplState -> Int -> CST.CST -> IO (FilePath, ReplState)
compileExprs st roundNum sourceExprs = do
  exprs <- case Mod.desugarTopLevel sourceExprs of
    Left e -> error ("desugar error: " ++ e)
    Right e -> pure e
  case Resolve.resolve (rsScope st) exprs of
    Left e -> error ("resolve error: " ++ show e)
    Right resolved -> case TC.typecheckWith (rsEnvs st) (rsContext st) resolved of
      Left e -> error ("typecheck error: " ++ show e)
      Right (typed, roundEnvs) -> do
        let llProg = LL.lambdaLift (CC.closureConvert typed)
            ir = Codegen.codegenRepl roundNum (rsGlobals st) (rsImportedMeta st) llProg
            soFile = "/tmp/pll_repl_" ++ show roundNum ++ ".so"
            llFile = "/tmp/pll_repl_" ++ show roundNum ++ ".ll"
            bridgeFile = "/tmp/pll_repl_bridge.c"
        T.IO.writeFile llFile ir
        T.IO.writeFile bridgeFile Ty.ffiBridgeC
        (ec, _, err') <- readProcessWithExitCode "clang"
          [llFile, bridgeFile, "-shared", "-fPIC", "-o", soFile,
           "-lm", "-lpcre2-8", "-lgc", "-lffi"] ""
        case ec of
          ExitFailure _ -> error ("clang .so failed:\n" ++ err' ++ "\nIR:\n" ++ T.unpack ir)
          ExitSuccess -> do
            let allNewGlobals = Codegen.collectReplGlobals (LL.llExprs llProg)
                existingNames = S.fromList (map fst (rsGlobals st))
                newGlobals = filter (\(n, _) -> not (S.member n existingNames)) allNewGlobals
                newNames = S.fromList (map fst newGlobals)
                newSchemes = M.fromList [(n, TC.Forall S.empty t) | (n, t) <- newGlobals]
                nextState = st
                  { rsScope = S.union (rsScope st) newNames
                  , rsContext = M.union newSchemes (rsContext st)
                  , rsGlobals = rsGlobals st ++ newGlobals
                  , rsEnvs = TC.mergeTCEnvs (rsEnvs st) roundEnvs
                  }
            pure (soFile, nextState)

-- | Check if a CST expression is a definition that should be replayed in future rounds
isDefExpr :: CST.Expr -> Bool
isDefExpr (Loc.Located _ e) = case e of
  CST.ExprType {}        -> True
  CST.ExprFFI {}         -> True
  CST.ExprFFIStruct {}   -> True
  CST.ExprFFIVar {}      -> True
  CST.ExprFFIEnum {}     -> True
  CST.ExprFFICallback {} -> True
  _                      -> False

-- | Generate a C driver that dlopens each .so and calls pll_repl_entry
genDriver :: [FilePath] -> String
genDriver soFiles = unlines $
  [ "#include <dlfcn.h>"
  , "#include <stdio.h>"
  , "#include <stdlib.h>"
  , "#include <gc/gc.h>"
  , ""
  , "int main() {"
  , "    GC_init();"
  ] ++
  concatMap loadSo soFiles ++
  [ "    return 0;"
  , "}"
  ]
  where
    loadSo path =
      [ "    {"
      , "        void *h = dlopen(\"" ++ path ++ "\", RTLD_NOW | RTLD_GLOBAL);"
      , "        if (!h) { fprintf(stderr, \"dlopen: %s\\n\", dlerror()); return 1; }"
      , "        void (*fn)(void) = dlsym(h, \"pll_repl_entry\");"
      , "        if (!fn) { fprintf(stderr, \"dlsym: %s\\n\", dlerror()); return 1; }"
      , "        fn();"
      , "    }"
      ]
