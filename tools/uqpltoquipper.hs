{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad (forM_, zipWithM_)
import Control.Monad.Trans (lift)
import Data.Void (Void)
import GHC.IO.Handle
import qualified Quipper as Q
import qualified Quipper.Libraries.Qureg as Q
import System.IO
import Text.Printf (printf)

import Lens.Micro.GHC
import Lens.Micro.Mtl

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx
import Traq.Data.Default
import qualified Traq.Data.Symbolic as Sym

import qualified Traq.CQPL as CQPL
import qualified Traq.Compiler.Unitary as CompileU
import Traq.Prelude
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

-- ================================================================================
-- Conversion
-- ================================================================================

type UnitaryCirc = [Q.Qureg] -> Q.Circ [Q.Qureg]

vartypeToQureg :: P.VarType SizeT -> Q.Qureg
vartypeToQureg (P.Fin n) = Q.qureg_shape logn
 where
  logn = ceiling $ log (fromIntegral n :: Double)

type ConversionCtx holeT sizeT = (Ctx.Context UnitaryCirc, Ctx.Context Q.Qureg)

circs :: Lens' (ConversionCtx holeT sizeT) (Ctx.Context UnitaryCirc)
circs = _1

quregs :: Lens' (ConversionCtx holeT sizeT) (Ctx.Context Q.Qureg)
quregs = _2

type ConverterT holeT sizeT = MyStateT (ConversionCtx holeT sizeT) Q.Circ

unitaryToCirc :: (Show sizeT) => CQPL.Unitary sizeT -> [Q.Qureg] -> Q.Circ ()
unitaryToCirc CQPL.Toffoli [a, b, c] = do
  let [qa] = Q.qulist_of_qureg_te a
  let [qb] = Q.qulist_of_qureg_te b
  let [qc] = Q.qulist_of_qureg_te c
  Q.gate_X_at qc `Q.controlled` (qa, qb)
unitaryToCirc (CQPL.RevEmbedU [a] (P.VarE a')) [arg, res]
  | a == a' =
      zipWithM_
        Q.controlled_not_at
        (Q.qulist_of_qureg_te arg)
        (Q.qulist_of_qureg_te res)
unitaryToCirc u _ = fail $ "invalid unitary " <> show u

stmtToCirc :: (Show holeT, Show sizeT) => CQPL.UStmt holeT sizeT -> ConverterT holeT sizeT ()
stmtToCirc CQPL.USkipS = return ()
stmtToCirc (CQPL.UCommentS _) = return ()
stmtToCirc (CQPL.USeqS ss) = mapM_ stmtToCirc ss
stmtToCirc CQPL.UnitaryS{unitary, args} = do
  qs <- zoom quregs $ mapM Ctx.unsafeLookup args
  lift $ unitaryToCirc unitary qs
stmtToCirc CQPL.UCallS{proc_id, dagger, args} = do
  qs <- zoom quregs $ mapM Ctx.unsafeLookup args
  circ <- zoom circs $ Ctx.unsafeLookup proc_id
  let circ_d = if dagger then Q.reverse_generic_endo circ else circ
  qs' <- lift $ circ_d qs
  forM_ (zip args qs') $ \(x, q) -> quregs . Ctx.ins x .= q
stmtToCirc (CQPL.URepeatS _ _) = fail "TODO repeat"
stmtToCirc CQPL.UHoleS{hole} = do
  q <- use $ quregs . to Ctx.elems
  lift $ Q.named_gate_at ("HOLE :: " ++ show hole) q
stmtToCirc CQPL.UForInRangeS{} = fail "TODO ForInRange"

type ProcConverterT holeT sizeT = MyStateT (Ctx.Context UnitaryCirc) Q.Circ

procDefToCirc :: (Show holeT, Show sizeT, Show costT) => CQPL.ProcDef holeT sizeT costT -> ProcConverterT holeT sizeT UnitaryCirc
procDefToCirc CQPL.UProcDef{proc_name, proc_body_or_tick = Left tick} =
  return $ Q.named_gate $ printf "%s[%s]" proc_name (show tick)
procDefToCirc CQPL.UProcDef{proc_name, proc_params, proc_body_or_tick = Right body} = do
  cs <- use id
  return $
    Q.box proc_name $ \qs -> do
      let vars = Ctx.fromList $ zip (map (view _1) proc_params) qs
      evalMyStateT (stmtToCirc body) (cs, vars)
      return qs

programToCirc ::
  forall holeT sizeT costT.
  (Show holeT, Show sizeT, Show costT) =>
  Ctx.Context (P.VarType sizeT) ->
  CQPL.Program holeT sizeT costT ->
  UnitaryCirc
programToCirc gamma CQPL.Program{CQPL.proc_defs, CQPL.stmt} qins = do
  Q.label qins (Ctx.keys gamma)
  procs <- execMyStateT convProcs Ctx.empty
  evalMyStateT (stmtToCirc stmt) (procs, Ctx.fromList $ zip (Ctx.keys gamma) qins)
  return qins
 where
  convProcs :: ProcConverterT holeT sizeT ()
  convProcs = do
    forM_ proc_defs $ \p -> do
      c <- procDefToCirc p
      Ctx.ins (CQPL.proc_name p) .= c

-- ================================================================================
-- Main
-- ================================================================================
eitherToIO :: (Show e) => Either e a -> IO a
eitherToIO (Right a) = return a
eitherToIO (Left e) = fail $ show e

redirectToFile :: FilePath -> IO a -> IO a
redirectToFile path action = do
  originalStdout <- hDuplicate stdout
  withFile path WriteMode $ \fileHandle -> do
    hDuplicateTo fileHandle stdout
    result <- action
    hFlush stdout
    hDuplicateTo originalStdout stdout
    return result

testExample :: IO ()
testExample = do
  -- a simple program
  prog <-
    unlines
      [ "declare Oracle(Fin<8>, Fin<8>) -> Bool"
      , "def Func(x : Fin<8>, y: Fin<8>) do"
      , "  b <- Oracle(x, y);"
      , "  return b : Bool"
      , "end"
      , "res <- Func(p, q)"
      ]
      & P.parseProgram @Void
      & eitherToIO
      <&> fmap Sym.unSym

  putStrLn $ replicate 40 '='
  putStrLn $ PP.toCodeString prog
  putStrLn $ replicate 40 '='

  -- compile to unitary CQPL
  let delta = 0.001 :: Double
  let gamma =
        Ctx.empty
          & (Ctx.ins "p" .~ P.Fin 8)
          & (Ctx.ins "q" .~ P.Fin 8)
  let oracle_ticks = mempty & at "Oracle" ?~ 1.0
  (uprog, gamma') <- eitherToIO $ CompileU.lowerProgram @_ @Void default_ gamma oracle_ticks delta prog

  putStrLn $ replicate 40 '='
  putStrLn $ PP.toCodeString uprog
  putStrLn $ replicate 40 '='

  -- convert to quipper
  let circ = programToCirc gamma' uprog
  let regs = gamma' & Ctx.elems & map vartypeToQureg
  redirectToFile "./example_quipper.pdf" $ do
    Q.print_generic Q.PDF circ regs
  Q.print_generic Q.ASCII circ regs

main :: IO ()
main = do
  putStrLn "Tool: convert unitary programs in CQPL to quipper."
  testExample
