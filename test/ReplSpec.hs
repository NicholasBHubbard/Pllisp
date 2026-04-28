{-# LANGUAGE OverloadedStrings #-}

module ReplSpec (spec) where

import Test.Hspec

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
  , rsPrevExprs :: [CST.Expr]  -- accumulated defs (TYPE, FFI, etc.) to replay
  }

-- | Run a sequence of REPL rounds, return the combined stdout output.
runRepl :: [T.Text] -> IO String
runRepl rounds = do
  preludeSexprs <- Stdlib.loadPrelude
  stRef <- newIORef (ReplState S.empty M.empty [] [])
  soFiles <- mapM (\(i, src) -> compileRound preludeSexprs stRef i src) (zip [0 :: Int ..] rounds)
  let driverSrc = genDriver soFiles
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

compileRound :: [SExpr.SExpr] -> IORef ReplState -> Int -> T.Text -> IO FilePath
compileRound preludeSexprs stRef roundNum src = do
  st <- readIORef stRef
  let sexprs = case Parser.parseSExprs "<repl>" src of
        Left e -> error ("parse error: " ++ show e)
        Right s -> s
      expanded = case MacroExpand.expandWith preludeSexprs sexprs of
        Left e -> error ("macro error: " ++ e)
        Right s -> s
      prog = case SExpr.toProgram expanded of
        Left e -> error ("sexpr error: " ++ SExpr.ceMsg e)
        Right p -> p
      -- Include accumulated defs (TYPE declarations etc.) before this round's exprs
  exprs <- case Mod.desugarTopLevel (rsPrevExprs st ++ CST.progExprs prog) of
    Left e -> error ("desugar error: " ++ e)
    Right e -> pure e
  case Resolve.resolve (rsScope st) exprs of
    Left e -> error ("resolve error: " ++ show e)
    Right resolved -> case TC.typecheck (rsContext st) resolved of
      Left e -> error ("typecheck error: " ++ show e)
      Right typed -> do
        let llProg = LL.lambdaLift (CC.closureConvert typed)
            ir = Codegen.codegenRepl roundNum (rsGlobals st) llProg
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
            -- Update state with new globals and defs
            let allNewGlobals = Codegen.collectReplGlobals (LL.llExprs llProg)
                existingNames = S.fromList (map fst (rsGlobals st))
                newGlobals = filter (\(n, _) -> not (S.member n existingNames)) allNewGlobals
                newNames = S.fromList (map fst newGlobals)
                -- Build schemes for new globals (monomorphic)
                newSchemes = M.fromList [(n, TC.Forall S.empty t) | (n, t) <- newGlobals]
                -- Accumulate TYPE/FFI defs for future rounds
                newDefs = filter isDefExpr (CST.progExprs prog)
            writeIORef stRef st
              { rsScope = S.union (rsScope st) newNames
              , rsContext = M.union newSchemes (rsContext st)
              , rsGlobals = rsGlobals st ++ newGlobals
              , rsPrevExprs = rsPrevExprs st ++ newDefs
              }
            pure soFile

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
