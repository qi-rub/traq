{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad (forM_, zipWithM_)
import Control.Monad.Trans (lift)
import Data.Void (Void)
import GHC.IO.Handle
import Lens.Micro.GHC
import Lens.Micro.Mtl
import System.IO

import QCompose.Control.Monad
import qualified QCompose.Data.Context as Ctx
import qualified QCompose.Data.Symbolic as Sym

import QCompose.Prelude
import qualified QCompose.ProtoLang as P
import qualified QCompose.UnitaryQPL as UQPL
import QCompose.Utils.Printing (toCodeString)

import qualified Quipper as Q
import qualified Quipper.Libraries.Qureg as Q

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

unitaryToCirc :: (Show sizeT) => UQPL.Unitary sizeT -> [Q.Qureg] -> Q.Circ ()
unitaryToCirc UQPL.Toffoli [a, b, c] = do
  let [qa] = Q.qulist_of_qureg_te a
  let [qb] = Q.qulist_of_qureg_te b
  let [qc] = Q.qulist_of_qureg_te c
  Q.gate_X_at qc `Q.controlled` (qa, qb)
unitaryToCirc (UQPL.RevEmbedU UQPL.IdF{}) [arg, res] =
  zipWithM_
    Q.controlled_not_at
    (Q.qulist_of_qureg_te arg)
    (Q.qulist_of_qureg_te res)
unitaryToCirc u _ = fail $ "invalid unitary " <> show u

stmtToCirc :: (Show holeT, Show sizeT) => UQPL.Stmt holeT sizeT -> ConverterT holeT sizeT ()
stmtToCirc UQPL.SkipS = return ()
stmtToCirc (UQPL.CommentS _) = return ()
stmtToCirc (UQPL.SeqS ss) = mapM_ stmtToCirc ss
stmtToCirc UQPL.UnitaryS{unitary, args} = do
  qs <- zoom quregs $ mapM Ctx.unsafeLookup args
  lift $ unitaryToCirc unitary qs
stmtToCirc UQPL.CallS{proc_id, dagger, args} = do
  qs <- zoom quregs $ mapM Ctx.unsafeLookup args
  circ <- zoom circs $ Ctx.unsafeLookup proc_id
  let circ_d = if dagger then Q.reverse_generic_endo circ else circ
  qs' <- lift $ circ_d qs
  forM_ (zip args qs') $ \(x, q) -> quregs . Ctx.ins x .= q
stmtToCirc (UQPL.RepeatS _ _) = fail "TODO repeat"
stmtToCirc UQPL.HoleS{hole} = do
  q <- use $ quregs . to Ctx.elems
  lift $ Q.named_gate_at ("HOLE :: " ++ show hole) q
stmtToCirc UQPL.ForInRangeS{} = fail "TODO ForInRange"

type ProcConverterT holeT sizeT = MyStateT (Ctx.Context UnitaryCirc) Q.Circ

procDefToCirc :: (Show holeT, Show sizeT) => UQPL.ProcDef holeT sizeT -> ProcConverterT holeT sizeT UnitaryCirc
procDefToCirc UQPL.ProcDef{proc_name, mproc_body = Nothing} =
  return $ Q.named_gate proc_name
procDefToCirc UQPL.ProcDef{proc_name, proc_params, mproc_body = Just body} = do
  cs <- use id
  return $
    Q.box proc_name $ \qs -> do
      let vars = Ctx.fromList $ zip (map (view _1) proc_params) qs
      evalMyStateT (stmtToCirc body) (cs, vars)
      return qs

programToCirc ::
  forall holeT sizeT.
  (Show holeT, Show sizeT) =>
  Ctx.Context (P.VarType sizeT) ->
  UQPL.Program holeT sizeT ->
  UnitaryCirc
programToCirc gamma UQPL.Program{UQPL.proc_defs, UQPL.stmt} qins = do
  Q.label qins (Ctx.keys gamma)
  procs <- execMyStateT convProcs Ctx.empty
  evalMyStateT (stmtToCirc stmt) (procs, Ctx.fromList $ zip (Ctx.keys gamma) qins)
  return qins
 where
  convProcs :: ProcConverterT holeT sizeT ()
  convProcs = do
    forM_ proc_defs $ \p -> do
      c <- procDefToCirc p
      Ctx.ins (UQPL.proc_name p) .= c

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
  putStrLn $ toCodeString prog
  putStrLn $ replicate 40 '='

  -- compile to UQPL
  let delta = 0.001 :: Double
  let gamma =
        Ctx.empty
          & Ctx.ins "p"
          .~ P.Fin 8
          & Ctx.ins "q"
          .~ P.Fin 8
  (uprog, gamma') <- eitherToIO $ UQPL.lowerProgram @_ @Void gamma "Oracle" delta prog

  putStrLn $ replicate 40 '='
  putStrLn $ toCodeString uprog
  putStrLn $ replicate 40 '='

  -- convert to quipper
  let circ = programToCirc gamma' uprog
  let regs = gamma' & Ctx.elems & map vartypeToQureg
  redirectToFile "./example_quipper.pdf" $ do
    Q.print_generic Q.PDF circ regs
  Q.print_generic Q.ASCII circ regs

main :: IO ()
main = do
  putStrLn "Tool: convert UQPL programs to quipper."
  testExample
