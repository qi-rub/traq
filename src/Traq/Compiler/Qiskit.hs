{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

{- HLINT ignore "Use camelCase" -}

module Traq.Compiler.Qiskit (
  toPy,
) where

import Control.Monad.Except (runExceptT)
import Control.Monad.RWS (RWS, runRWS)
import Data.List (intersperse)
import qualified Data.Set as Set
import Prettyprinter ((<+>))
import qualified Prettyprinter as PP

import Lens.Micro.GHC
import Lens.Micro.Mtl

import qualified Traq.Data.Context as Ctx
import Traq.Data.Default

import qualified Traq.CPL as CPL
import Traq.Compiler.Python
import Traq.Prelude
import qualified Traq.QPL as QPL

-- ============================================================
-- Compile QPL -> py (+Qiskit)
-- ============================================================

newtype QiskitState = QiskitState (Set.Set Ident)
  deriving (HasDefault)

_externDefNames :: Lens' QiskitState (Set.Set Ident)
_externDefNames focus (QiskitState s) = focus s <&> QiskitState

-- | Build python code string.
class ToQiskitPy a where
  type Ctx a

  mkPy :: a -> RWS (Ctx a) () QiskitState (Py ann)

-- | Convert a QPL program to a python code string.
toPy :: QPL.Program SizeT -> String
toPy prog =
  let (pyDoc, _, _) = runRWS (mkPy prog) () default_
   in show pyDoc

-- ============================================================
-- Basic Instances
-- ============================================================

instance (Show size, Integral size) => ToQiskitPy (QPL.Program size) where
  type Ctx (QPL.Program size) = ()

  mkPy (QPL.Program ps) = do
    code <- PP.vsep . intersperse PP.line <$> mapM mkPy ps
    exts <- py_tupled . map (PP.dquotes . PP.pretty) . Set.toList <$> use _externDefNames
    pure $
      PP.vsep
        [ code
        , PP.pretty "EXTERN_DEFS" <+> PP.equals <+> exts
        , PP.pretty "ENTRY_POINT" <+> PP.equals <+> (PP.dquotes . PP.pretty . QPL.proc_name $ last ps)
        ]

instance (Show size, Integral size) => ToQiskitPy (QPL.ProcDef size) where
  type Ctx (QPL.ProcDef size) = ()

  mkPy QPL.ProcDef{proc_name, proc_meta_params, proc_param_types, proc_body} =
    PP.vsep
      <$> sequence
        [ withEnv
            (ProcBuildCtx{..})
            (mkPy proc_body)
        ]

data ProcBuildCtx size = ProcBuildCtx
  { proc_name :: Ident
  , proc_meta_params :: [Ident]
  , proc_param_types :: [CPL.VarType size]
  }
  deriving (Read, Show, Eq)

instance (Show size, Integral size) => ToQiskitPy (QPL.ProcBody size) where
  type Ctx (QPL.ProcBody size) = ProcBuildCtx size

  mkPy (QPL.ProcBodyU ubody) = mkPy ubody
  mkPy (QPL.ProcBodyC cbody) = mkPy cbody

-- ============================================================
-- Qiskit helpers
-- ============================================================

-- | Emit a black-box gate spanning all qubits. TODO: implement properly.
blackbox :: String -> Py ann
blackbox name =
  PP.pretty "qc.append"
    <> PP.tupled
      [ PP.pretty "qiskit.circuit.Gate" <> PP.tupled [PP.dquotes (PP.pretty name), PP.pretty "qc.num_qubits", PP.pretty "[]"]
      , PP.pretty "qc.qubits"
      ]

-- | Emit a custom named gate with a given qubit count.
customGate :: (Show size) => String -> size -> Py ann
customGate name n =
  PP.pretty "qiskit.circuit.Gate"
    <> PP.tupled [PP.dquotes (PP.pretty name), PP.pretty (show n), PP.pretty "[]"]

-- | Emit a Qiskit library gate constructor.
libGate :: String -> Py ann
libGate name = PP.pretty $ "qiskit.circuit.library." <> name <> "()"

-- | Emit a parameterized Qiskit library gate constructor.
libGateParam :: String -> String -> Py ann
libGateParam name param = PP.pretty $ "qiskit.circuit.library." <> name <> "(" <> param <> ")"

{- | Generate qubit reference for a QPL argument with type-aware slicing.
For whole registers: *reg
For array elements: *reg[start:end] (slice for the element's qubit range)
-}
py_qarg :: (Show size, Integral size) => QPL.Arg size -> CPL.VarType size -> Py ann
py_qarg (QPL.Arg x) _ = PP.pretty "*" <> py_sanitizeIdent x
py_qarg (QPL.ArrElemArg (QPL.Arg x) (CPL.MetaSize i)) elemTy =
  let s = CPL.bestBitsize elemTy
      start = fromIntegral i * fromIntegral s :: Integer
      end = (fromIntegral i + 1) * fromIntegral s :: Integer
   in PP.pretty "*" <> py_sanitizeIdent x <> PP.brackets (PP.pretty (show start) <> PP.colon <> PP.pretty (show end))
py_qarg (QPL.ArrElemArg (QPL.Arg x) (CPL.MetaName n)) elemTy =
  let s = CPL.bestBitsize elemTy
   in if s == 1
        then PP.pretty "*" <> py_sanitizeIdent x <> PP.brackets (py_sanitizeIdent n <> PP.colon <> py_sanitizeIdent n <+> PP.pretty "+" <+> PP.pretty "1")
        else
          PP.pretty "*"
            <> py_sanitizeIdent x
            <> PP.brackets
              ( py_sanitizeIdent n
                  <+> PP.pretty "*"
                  <+> PP.pretty (show s)
                  <> PP.colon
                  <> PP.parens (py_sanitizeIdent n <+> PP.pretty "+" <+> PP.pretty "1")
                  <+> PP.pretty "*"
                  <+> PP.pretty (show s)
              )
py_qarg arg _ = PP.pretty "*" <> py_arg arg

-- ============================================================
-- Unitary: Emit Qiskit unitary circuits
-- ============================================================

instance (Show size, Integral size) => ToQiskitPy (QPL.UProcBody size) where
  type Ctx (QPL.UProcBody size) = ProcBuildCtx size

  mkPy QPL.UProcDecl = do
    ProcBuildCtx{..} <- view id
    let uproc_param_names = ["q_" <> show i | i <- [1 .. length proc_param_types]]
    let param_defs =
          [ py_sanitizeIdent p
              <+> PP.equals
              <+> PP.pretty "qiskit.circuit.Parameter"
              <> PP.parens (PP.dquotes (py_sanitizeIdent p))
          | p <- proc_meta_params
          ]
    let reg_defs =
          [ py_sanitizeIdent n
              <+> PP.equals
              <+> PP.pretty "qiskit.circuit.QuantumRegister"
              <> PP.tupled [PP.pretty (show $ CPL.bestBitsize ty), PP.dquotes (py_sanitizeIdent n)]
          | (n, ty) <- zip uproc_param_names proc_param_types
          ]
    let reg_names = map py_sanitizeIdent uproc_param_names
    let qc_def =
          PP.pretty "qc"
            <+> PP.equals
            <+> PP.pretty "qiskit.circuit.QuantumCircuit"
            <> PP.tupled (reg_names ++ [PP.pretty "name=" <> PP.dquotes (py_sanitizeIdent proc_name)])
    let body =
          PP.vsep $
            param_defs
              ++ reg_defs
              ++ [ qc_def
                 , PP.pretty "return qc"
                 ]
    pure $ py_def proc_name [] body
  mkPy QPL.UProcBody{uproc_param_names, uproc_body_stmt} = do
    ProcBuildCtx{..} <- view id
    let param_defs =
          [ py_sanitizeIdent p
              <+> PP.equals
              <+> PP.pretty "qiskit.circuit.Parameter"
              <> PP.parens (PP.dquotes (py_sanitizeIdent p))
          | p <- proc_meta_params
          ]
    let reg_defs =
          [ py_sanitizeIdent n
              <+> PP.equals
              <+> PP.pretty "qiskit.circuit.QuantumRegister"
              <> PP.tupled [PP.pretty (show $ CPL.bestBitsize ty), PP.dquotes (py_sanitizeIdent n)]
          | (n, ty) <- zip uproc_param_names proc_param_types
          ]
    let reg_names = map py_sanitizeIdent uproc_param_names
    let qc_def =
          PP.pretty "qc"
            <+> PP.equals
            <+> PP.pretty "qiskit.circuit.QuantumCircuit"
            <> PP.tupled (reg_names ++ [PP.pretty "name=" <> PP.dquotes (py_sanitizeIdent proc_name)])
    let typCtx = Ctx.fromList (zip uproc_param_names proc_param_types)
    stmt_body <- withEnv typCtx $ mkPy uproc_body_stmt
    let body =
          PP.vsep $
            param_defs
              ++ reg_defs
              ++ [ mempty
                 , qc_def
                 , mempty
                 , stmt_body
                 , mempty
                 , PP.pretty "return qc"
                 ]
    pure $ py_def proc_name [] body

instance (Show size, Integral size) => ToQiskitPy (QPL.UStmt size) where
  type Ctx (QPL.UStmt size) = CPL.TypingCtx size

  mkPy QPL.USkipS = pure mempty
  mkPy (QPL.UCommentS s) = pure $ py_comment s
  mkPy QPL.UnitaryS{qargs, unitary} = do
    tys <- fmap (either (error . show) id) . runExceptT $ do
      mapM QPL.getArgTy qargs
    gateExpr <- withEnv tys $ mkPy unitary
    let qubits = PP.hsep $ PP.punctuate PP.comma $ zipWith py_qarg qargs tys
    pure $ PP.pretty "qc.append" <> PP.tupled [gateExpr, PP.brackets qubits]
  mkPy QPL.UCallS{uproc_id, dagger, qargs} = do
    let gate = py_sanitizeIdent uproc_id <> PP.pretty "().to_gate()"
    let gateExpr = if dagger then gate <> PP.pretty ".inverse()" else gate
    let qubits = PP.hsep $ PP.punctuate PP.comma [PP.pretty "*" <> py_arg q | q <- qargs]
    pure $ PP.pretty "qc.append" <> PP.tupled [gateExpr, PP.brackets qubits]
  mkPy (QPL.USeqS ss) = PP.vsep <$> mapM mkPy ss
  mkPy QPL.URepeatS{n_iter, uloop_body} = do
    body <- mkPy uloop_body
    let n = py_metaParam (Left n_iter)
    pure $
      PP.vsep
        [ PP.pretty "with qc.for_loop" <> PP.parens (PP.pretty "range" <> PP.parens n) <> PP.colon
        , py_indent body
        ]
  mkPy QPL.UForInRangeS{} = pure $ blackbox "UForInRangeS"
  mkPy QPL.UForInDomainS{} = pure $ blackbox "UForInDomainS"
  mkPy QPL.UWithComputedS{} = pure $ blackbox "UWithComputedS"

instance (Show size, Integral size) => ToQiskitPy (QPL.Unitary Double size) where
  type Ctx (QPL.Unitary Double size) = [CPL.VarType size]

  mkPy (QPL.BasicGateU g) = mkPy g
  mkPy (QPL.DistrU d) = do
    tys <- view id
    let n = sum $ map CPL.bestBitsize tys
    let name = filter (\c -> c /= '"' && c /= '\\') $ show d
    pure $ customGate ("DistrU (" ++ name ++ ")") n
  mkPy (QPL.Controlled u) = do
    inner <- mkPy u
    pure $ inner <> PP.pretty ".control(1)"
  mkPy (QPL.Adjoint u) = do
    inner <- mkPy u
    pure $ inner <> PP.pretty ".inverse()"
  mkPy (QPL.RevEmbedU xs e) = do
    tys <- view id
    let n = sum $ map CPL.bestBitsize tys
    let name = filter (\c -> c /= '"' && c /= '\\') $ show (QPL.RevEmbedU xs e :: QPL.Unitary Double size)
    pure $ customGate name n
  mkPy (QPL.NamedGateU name) = do
    tys <- view id
    let n = sum $ map CPL.bestBitsize tys
    pure $ customGate name n

instance (Show size, Integral size) => ToQiskitPy (QPL.BasicGate size) where
  type Ctx (QPL.BasicGate size) = [CPL.VarType size]

  mkPy QPL.XGate = pure $ libGate "XGate"
  mkPy QPL.HGate = pure $ libGate "HGate"
  mkPy QPL.ZGate = pure $ libGate "ZGate"
  mkPy QPL.CNOT = pure $ libGate "CXGate"
  mkPy QPL.Toffoli = pure $ libGate "CCXGate"
  mkPy QPL.SWAP = pure $ libGate "SwapGate"
  mkPy QPL.COPY = pure $ libGate "CXGate"
  mkPy (QPL.Rz theta) = pure $ libGateParam "RZGate" (show theta)
  mkPy (QPL.PhaseOnZero theta) = do
    tys <- view id
    let n = sum $ map CPL.bestBitsize tys
    pure $ customGate ("PhaseOnZero(" ++ show theta ++ ")") n
  mkPy QPL.Unif = error "TODO Unif"

-- ============================================================
-- Classical: Emit Qiskit circuits with control-flow
-- ============================================================

instance (Show size, Integral size) => ToQiskitPy (QPL.CProcBody size) where
  type Ctx (QPL.CProcBody size) = ProcBuildCtx size

  -- external
  mkPy QPL.CProcDecl = do
    ProcBuildCtx{..} <- view id
    let cproc_param_names = ["c_" <> show i | i <- [1 .. length proc_param_types]]
    let param_defs =
          [ py_sanitizeIdent p
              <+> PP.equals
              <+> PP.pretty "qiskit.circuit.Parameter"
              <> PP.parens (PP.dquotes (py_sanitizeIdent p))
          | p <- proc_meta_params
          ]
    let reg_defs =
          [ py_sanitizeIdent n
              <+> PP.equals
              <+> PP.pretty "qiskit.circuit.ClassicalRegister"
              <> PP.tupled [PP.pretty (show $ CPL.bestBitsize ty), PP.dquotes (py_sanitizeIdent n)]
          | (n, ty) <- zip cproc_param_names proc_param_types
          ]
    let reg_names = map py_sanitizeIdent cproc_param_names
    let qc_def =
          PP.pretty "qc"
            <+> PP.equals
            <+> PP.pretty "qiskit.circuit.QuantumCircuit"
            <> PP.tupled (reg_names ++ [PP.pretty "name=" <> PP.dquotes (py_sanitizeIdent proc_name)])
    let body =
          PP.vsep $
            param_defs
              ++ reg_defs
              ++ [ qc_def
                 , PP.pretty "return qc"
                 ]
    pure $ py_def proc_name [] body

  -- defined
  mkPy QPL.CProcBody{cproc_param_names, cproc_local_vars, cproc_body_stmt} = do
    ProcBuildCtx{..} <- view id
    let param_defs =
          [ py_sanitizeIdent p
              <+> PP.equals
              <+> PP.pretty "qiskit.circuit.Parameter"
              <> PP.parens (PP.dquotes (py_sanitizeIdent p))
          | p <- proc_meta_params
          ]
    let all_vars = zip cproc_param_names proc_param_types ++ cproc_local_vars
    let reg_defs =
          [ py_sanitizeIdent n
              <+> PP.equals
              <+> PP.pretty "qiskit.circuit.ClassicalRegister"
              <> PP.tupled [PP.pretty (show $ CPL.bestBitsize ty), PP.dquotes (py_sanitizeIdent n)]
          | (n, ty) <- all_vars
          ]
    let reg_names = map (py_sanitizeIdent . fst) all_vars
    let qc_def =
          PP.pretty "qc"
            <+> PP.equals
            <+> PP.pretty "qiskit.circuit.QuantumCircuit"
            <> PP.tupled (reg_names ++ [PP.pretty "name=" <> PP.dquotes (py_sanitizeIdent proc_name)])
    stmt_body <- withEnv () $ mkPy cproc_body_stmt
    let body =
          PP.vsep $
            param_defs
              ++ reg_defs
              ++ [ qc_def
                 , stmt_body
                 , PP.pretty "return qc"
                 ]
    pure $ py_def proc_name [] body

instance (Show size) => ToQiskitPy (QPL.Stmt size) where
  type Ctx (QPL.Stmt size) = ()

  mkPy QPL.SkipS = pure py_pass
  mkPy (QPL.CommentS s) = pure $ py_comment s
  mkPy QPL.AssignS{rets, expr} = do
    let lhs = PP.hsep $ PP.punctuate PP.comma (map py_sanitizeIdent rets)
    pure $ lhs <+> PP.equals <+> py_expr expr
  mkPy QPL.RandomS{} = pure $ blackbox "RandomS"
  mkPy QPL.RandomDynS{ret, max_var} =
    pure $ PP.pretty ret <+> PP.equals <+> PP.pretty "random.randrange" <> PP.parens (PP.pretty max_var)
  mkPy QPL.CallS{fun = QPL.FunctionCall proc_id, args} = do
    let instr = py_sanitizeIdent proc_id <> PP.pretty "().to_instruction()"
    let cbits = PP.hsep $ PP.punctuate PP.comma [PP.pretty "*" <> py_arg q | q <- args]
    pure $ PP.pretty "qc.append" <> PP.tupled [instr, PP.pretty "[]", PP.brackets cbits]
  mkPy QPL.CallS{fun = QPL.UProcAndMeas{}} = pure $ blackbox "UProcAndMeas"
  mkPy (QPL.SeqS ss) = PP.vsep <$> mapM mkPy ss
  mkPy QPL.IfThenElseS{cond, s_true, s_false} = py_ifte cond <$> mkPy s_true <*> mkPy s_false
  mkPy QPL.RepeatS{n_iter, loop_body} = do
    body <- mkPy loop_body
    let n = py_metaParam (Left n_iter)
    pure $
      PP.vsep
        [ PP.pretty "for _ in range" <> PP.parens n <> PP.colon
        , py_indent body
        ]
  mkPy QPL.WhileK{} = pure $ blackbox "WhileK"
  mkPy QPL.WhileKWithCondExpr{} = pure $ blackbox "WhileKWithCondExpr"
  mkPy QPL.ForInArray{loop_index, loop_values, loop_body} = do
    body <- mkPy loop_body
    let vals = PP.list (map py_expr loop_values)
    pure $
      PP.vsep
        [ PP.pretty "for" <+> PP.pretty loop_index <+> PP.pretty "in" <+> vals <> PP.colon
        , py_indent body
        ]
  mkPy QPL.ForInRangeS{} = pure $ blackbox "ForInRangeS"
  mkPy QPL.BlackBoxS{} = pure $ blackbox "BlackBoxS"
