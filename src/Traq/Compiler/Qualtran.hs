{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Use camelCase" -}

module Traq.Compiler.Qualtran (
  toPy,
) where

import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (Reader, ReaderT (..), runReader)
import Data.List (intersperse)
import Prettyprinter ((<+>))
import qualified Prettyprinter as PP
import Text.Printf (printf)

import Lens.Micro.GHC
import Lens.Micro.Mtl

import qualified Traq.Data.Context as Ctx

import qualified Traq.CPL as CPL
import Traq.Prelude
import qualified Traq.QPL as QPL

-- ============================================================
-- Compile QPL -> py (+Qualtran)
-- ============================================================

type Py ann = PP.Doc ann

-- | Build python code string.
class ToQualtranPy a where
  type Ctx a

  mkPy :: a -> Reader (Ctx a) (Py ann)

-- | Convert a QPL program to a python code string.
toPy :: QPL.Program SizeT -> String
toPy prog =
  let pyDoc = runReader (mkPy prog) ()
   in show pyDoc

-- ============================================================
-- Helpers for building python syntax
-- ============================================================

withEnv :: (Monad m) => r -> ReaderT r m a -> ReaderT r' m a
withEnv r = magnify (lens (const r) const)

py_indent :: Py ann -> Py ann
py_indent = PP.indent tabwidth
 where
  tabwidth :: Int
  tabwidth = 4

-- ------------------------------------------------------------
-- variables and types
-- ------------------------------------------------------------
py_sanitizeIdent :: Ident -> Py ann
py_sanitizeIdent s = PP.pretty $ map conv s
 where
  conv '\'' = '_'
  conv c = c

py_typedArg :: String -> Py ann -> Py ann
py_typedArg x t = py_sanitizeIdent x <+> PP.colon <+> t

-- ------------------------------------------------------------
-- statements
-- ------------------------------------------------------------

py_comment :: String -> Py ann
py_comment c = PP.vsep $ lines c <&> \l -> PP.pretty $ "# " <> l

py_pass :: Py ann
py_pass = PP.pretty "pass"

py_ifte :: String -> Py ann -> Py ann -> Py ann
py_ifte b s_t s_f =
  PP.vsep
    [ PP.pretty $ "if (" <> b <> "):"
    , py_indent s_t
    , PP.pretty "else:"
    , py_indent s_f
    ]

py_return :: [Py ann] -> Py ann
py_return args = PP.pretty "return" <+> PP.hsep (PP.punctuate PP.comma args)

py_raise_s :: String -> Py ann
py_raise_s e = PP.pretty @String $ printf "raise Exception('%s')" e

py_notImplemented :: String -> Py ann
py_notImplemented msg = PP.pretty "NotImplementedError" <> PP.parens (PP.dquotes $ PP.pretty msg)

py_def :: Ident -> [Py ann] -> Py ann -> Py ann
py_def name params body =
  PP.vsep
    [ PP.pretty "def" <+> PP.pretty name <> PP.tupled params <> PP.pretty ":"
    , py_indent body
    ]

py_class :: Ident -> Ident -> Py ann -> Py ann
py_class name parent body =
  PP.vsep
    [ PP.pretty "class" <+> PP.pretty name <> PP.parens (PP.pretty parent) <> PP.colon
    , py_indent body
    ]

py_decorator :: String -> Py ann
py_decorator s = PP.pretty $ "@" <> s

py_property :: Ident -> Py ann -> Py ann
py_property name body =
  PP.vsep
    [ py_decorator "property"
    , py_def name [PP.pretty "self"] body
    ]

bitsize :: (Integral a) => a -> a
bitsize n
  | n <= 1 = 1
  | otherwise = ceiling $ logBase (2 :: Double) (fromIntegral n)

-- | Map a VarType to a Qualtran QDType expression
toQltDType :: (Show size, Integral size) => CPL.VarType size -> Py ann
toQltDType (CPL.Fin n) =
  PP.pretty "qlt.BQUInt" <> PP.tupled [PP.pretty (show $ bitsize n), PP.pretty (show n)]
toQltDType (CPL.Bitvec n) = PP.pretty "qlt.QAny" <> PP.parens (PP.pretty (show n))
toQltDType (CPL.Tup _) = error "TODO toQltDType Tup"
toQltDType (CPL.Arr _ t) = toQltDType t -- base dtype; shape handled in py_register

-- | Build shape tuple for a VarType (only Arr adds dimensions)
toQltShape :: (Show size) => CPL.VarType size -> [Py ann]
toQltShape (CPL.Arr n t) = PP.pretty (show n) : toQltShape t
toQltShape _ = []

-- | Build a qlt.Register(...) expression
py_register :: (Show size, Integral size) => Ident -> CPL.VarType size -> Py ann
py_register name ty =
  let dtype = toQltDType ty
      shape = toQltShape ty
      shapeArg = case shape of
        [] -> []
        _ -> [PP.pretty "shape=" <> PP.tupled shape]
   in PP.pretty "qlt.Register" <> PP.tupled ([PP.dquotes (py_sanitizeIdent name), dtype] ++ shapeArg)

py_metaParam :: (Show size) => Either (CPL.MetaParam size) Ident -> Py ann
py_metaParam (Left (CPL.MetaName n)) = py_sanitizeIdent n
py_metaParam (Left (CPL.MetaSize s)) = PP.pretty (show s)
py_metaParam (Right name) = py_sanitizeIdent name

py_arg :: (Show size) => QPL.Arg size -> Py ann
py_arg (QPL.Arg x) = py_sanitizeIdent x
py_arg (QPL.ArrElemArg a i) = py_arg a <> PP.brackets (py_metaParam (Left i))

-- | Emit a python expression
py_expr :: (Show size) => CPL.BasicExpr size -> Py ann
py_expr CPL.VarE{var} = py_sanitizeIdent var
py_expr CPL.ParamE{param} = py_sanitizeIdent param
py_expr CPL.DefaultE{ty} = py_defaultVal ty
py_expr CPL.ConstE{val} = py_val val
py_expr CPL.UnOpE{un_op, operand} = py_unOp un_op <> PP.parens (py_expr operand)
py_expr CPL.BinOpE{bin_op, lhs, rhs} = PP.parens $ py_expr lhs <+> py_binOp bin_op <+> py_expr rhs
py_expr CPL.TernaryE{branch, lhs, rhs} = PP.parens $ py_expr lhs <+> PP.pretty "if" <+> py_expr branch <+> PP.pretty "else" <+> py_expr rhs
py_expr CPL.NAryE{op, operands} = py_naryOp op <> PP.tupled (map py_expr operands)
py_expr CPL.IndexE{arr_expr, ix_val} = py_expr arr_expr <> PP.brackets (PP.pretty (show ix_val))
py_expr CPL.DynIndexE{arr_expr, ix_expr} = py_expr arr_expr <> PP.brackets (py_expr ix_expr)
py_expr CPL.UpdateArrE{arr_expr, ix_expr, rhs} = error "TODO UpdateArrE"
py_expr CPL.ProjectE{tup_expr, tup_ix_val} = py_expr tup_expr <> PP.brackets (PP.pretty (show tup_ix_val))

py_val :: (Show size) => CPL.Value size -> Py ann
py_val (CPL.FinV n) = PP.pretty (show n)
py_val (CPL.ArrV vs) = PP.list (map py_val vs)
py_val (CPL.TupV vs) = PP.tupled (map py_val vs)

py_defaultVal :: (Show size) => CPL.VarType size -> Py ann
py_defaultVal (CPL.Fin _) = PP.pretty "0"
py_defaultVal (CPL.Bitvec _) = PP.pretty "0"
py_defaultVal (CPL.Arr n t) = PP.brackets (py_defaultVal t) <+> PP.pretty "*" <+> PP.pretty (show n)
py_defaultVal (CPL.Tup ts) = PP.tupled (map py_defaultVal ts)

py_unOp :: CPL.UnOp -> Py ann
py_unOp CPL.NotOp = PP.pretty "not "
py_unOp CPL.AnyOp = PP.pretty "any"
py_unOp CPL.AllOp = PP.pretty "all"
py_unOp CPL.MajOp = error "TODO MajOp"

py_binOp :: CPL.BinOp -> Py ann
py_binOp CPL.AddOp = PP.pretty "+"
py_binOp CPL.MulOp = PP.pretty "*"
py_binOp CPL.SubOp = PP.pretty "-"
py_binOp CPL.XorOp = PP.pretty "^"
py_binOp CPL.LEqOp = PP.pretty "<="
py_binOp CPL.LtOp = PP.pretty "<"
py_binOp CPL.AndOp = PP.pretty "and"
py_binOp CPL.EqOp = PP.pretty "=="
py_binOp CPL.VecSelectOp = error "TODO VecSelectOp"

py_naryOp :: CPL.NAryOp -> Py ann
py_naryOp CPL.MultiOrOp = PP.pretty "any"

-- ============================================================
-- Basic Instances
-- ============================================================

instance (Show size, Integral size) => ToQualtranPy (QPL.Program size) where
  type Ctx (QPL.Program size) = ()

  mkPy (QPL.Program ps) =
    PP.vsep . intersperse PP.line <$> mapM mkPy ps

instance (Show size, Integral size) => ToQualtranPy (QPL.ProcDef size) where
  type Ctx (QPL.ProcDef size) = ()

  mkPy QPL.ProcDef{info_comment, proc_name, proc_meta_params, proc_param_types, proc_body} =
    PP.vsep
      <$> sequence
        [ pure $ py_comment info_comment
        , withEnv
            (ProcBuildCtx{..})
            (mkPy proc_body)
        ]

data ProcBuildCtx size = ProcBuildCtx
  { proc_name :: Ident
  , proc_meta_params :: [Ident]
  , proc_param_types :: [CPL.VarType size]
  }
  deriving (Read, Show, Eq)

instance (Show size, Integral size) => ToQualtranPy (QPL.ProcBody size) where
  type Ctx (QPL.ProcBody size) = ProcBuildCtx size

  mkPy (QPL.ProcBodyU ubody) = mkPy ubody
  mkPy (QPL.ProcBodyC cbody) = mkPy cbody

-- ============================================================
-- Unitary: Emit Qualtran Bloqs
-- ============================================================

instance (Show size, Integral size) => ToQualtranPy (QPL.UProcBody size) where
  type Ctx (QPL.UProcBody size) = ProcBuildCtx size

  mkPy QPL.UProcDecl = do
    ProcBuildCtx{..} <- view id
    let meta_attrs = map (\p -> py_sanitizeIdent p <> PP.pretty ": int") proc_meta_params
    let regs = zipWith py_register uproc_param_names proc_param_types
         where
          uproc_param_names = ["q_" <> show i | i <- [1 .. length proc_param_types]]
    let sig_body = PP.pretty "return qlt.Signature" <> PP.parens (PP.list regs)
    let class_body =
          PP.vsep $
            intersperse
              PP.line
              [ PP.vsep meta_attrs
              , py_property "signature" sig_body
              ]
    pure $
      PP.vsep
        [ py_decorator "attrs.frozen"
        , py_class proc_name "qlt.Bloq" class_body
        ]
  mkPy QPL.UProcBody{uproc_param_names, uproc_param_tags, uproc_body_stmt} = do
    ProcBuildCtx{..} <- view id
    let meta_attrs = map (\p -> py_sanitizeIdent p <> PP.pretty ": int") proc_meta_params
    let regs = zipWith py_register uproc_param_names proc_param_types
    let sig_body = PP.pretty "return qlt.Signature" <> PP.parens (PP.list regs)

    let typCtx = Ctx.fromList (zip uproc_param_names proc_param_types)
    stmt_body <- withEnv typCtx $ mkPy uproc_body_stmt
    let bcb_params = PP.pretty "self" : PP.pretty "bb: qlt.BloqBuilder" : map py_sanitizeIdent uproc_param_names
    let bcb_body =
          PP.vsep
            [ stmt_body
            , PP.pretty "return"
                <+> PP.braces
                  ( PP.hsep $
                      PP.punctuate
                        PP.comma
                        [ PP.dquotes (py_sanitizeIdent n) <> PP.colon <+> py_sanitizeIdent n
                        | n <- uproc_param_names
                        ]
                  )
            ]

    let class_body =
          PP.vsep $
            intersperse
              PP.line
              [ PP.vsep meta_attrs
              , py_property "signature" sig_body
              , py_def "build_composite_bloq" bcb_params bcb_body
              ]
    pure $
      PP.vsep
        [ py_decorator "attrs.frozen"
        , py_class proc_name "qlt.Bloq" class_body
        ]

instance (Show size, Integral size) => ToQualtranPy (QPL.UStmt size) where
  type Ctx (QPL.UStmt size) = CPL.TypingCtx size

  mkPy QPL.USkipS = pure mempty
  mkPy (QPL.UCommentS s) = pure $ py_comment s
  mkPy QPL.UnitaryS{qargs, unitary} = do
    tys <- fmap (either (error . show) id) . runExceptT $ do
      mapM QPL.getArgTy qargs
    bloqExpr <- withEnv tys $ mkPy unitary
    let argVals = PP.list [py_arg q | q <- qargs]
    let lhs = PP.hsep $ PP.punctuate PP.comma [py_arg q | q <- qargs]
    pure $ lhs <+> PP.equals <+> PP.pretty "add_bloq" <> PP.tupled [PP.pretty "bb", bloqExpr, argVals]
  mkPy QPL.UCallS{uproc_id, dagger, qargs} = do
    let bloq = py_sanitizeIdent uproc_id <> PP.pretty "()"
    let bloqExpr = if dagger then bloq <> PP.pretty ".adjoint()" else bloq
    let argVals = PP.list [py_arg q | q <- qargs]
    let lhs = PP.hsep $ PP.punctuate PP.comma [py_arg q | q <- qargs]
    pure $ lhs <+> PP.equals <+> PP.pretty "add_bloq" <> PP.tupled [PP.pretty "bb", bloqExpr, argVals]
  -- compound statements
  mkPy (QPL.USeqS ss) = PP.vsep <$> mapM mkPy ss
  mkPy QPL.URepeatS{n_iter, uloop_body} = do
    body <- mkPy uloop_body
    let n = py_metaParam (Left n_iter)
    pure $
      PP.vsep
        [ PP.pretty "for _ in range" <> PP.parens n <> PP.colon
        , py_indent body
        ]
  mkPy QPL.UForInRangeS{iter_meta_var, iter_lim, dagger, uloop_body} = do
    body <- mkPy (if dagger then QPL.adjoint uloop_body else uloop_body)
    let n = py_metaParam (Left iter_lim)
    let range_expr =
          if dagger
            then PP.pretty "reversed(range" <> PP.parens n <> PP.pretty ")"
            else PP.pretty "range" <> PP.parens n
    pure $
      PP.vsep
        [ PP.pretty "for" <+> py_sanitizeIdent iter_meta_var <+> PP.pretty "in" <+> range_expr <> PP.colon
        , py_indent body
        ]
  mkPy QPL.UForInDomainS{iter_meta_var, iter_ty, dagger, uloop_body} = pure $ py_notImplemented "TODO UForInDomainS"
  mkPy QPL.UWithComputedS{with_ustmt, body_ustmt} = do
    mkPy with_ustmt
    mkPy body_ustmt
    mkPy (QPL.adjoint with_ustmt)

instance (Show size, Integral size) => ToQualtranPy (QPL.Unitary size) where
  type Ctx (QPL.Unitary size) = [CPL.VarType size]

  mkPy (QPL.BasicGateU g) = mkPy g
  mkPy (QPL.DistrU (CPL.UniformE ty)) = do
    let bs = CPL.bestBitsize ty
    pure $ PP.pretty "QFTTextBook" <> PP.tupled [PP.pretty (show bs)]
  mkPy (QPL.DistrU (CPL.BernoulliE p)) = do
    let theta = PP.pretty @String $ printf "%f" (2 * asin (sqrt p))
    pure $ PP.pretty "qlt_gates.Ry" <> PP.tupled [PP.pretty "angle=" <> theta]
  mkPy (QPL.Controlled u) = do
    bloq <- mkPy u
    pure $ bloq <> PP.pretty ".controlled()"
  mkPy (QPL.Adjoint u) = do
    bloq <- mkPy u
    pure $ bloq <> PP.pretty ".adjoint()"

  -- embed classical gates as unitaries
  mkPy (QPL.RevEmbedU xs e) = do
    tys <- view id
    let ctx = Ctx.fromList $ zip xs tys
    withEnv ctx $ exprToBloq e

namedBloq :: (Show size, Integral size) => Ident -> [CPL.VarType size] -> Py ann
namedBloq b ts =
  PP.pretty "NamedBloq"
    <> PP.tupled
      [ PP.dquotes $ PP.pretty b
      , PP.list [py_register ("x_" <> show i) t | (t, i) <- zip ts [0 ..]]
      ]

exprToBloq :: (Show size, Integral size) => CPL.BasicExpr size -> Reader (CPL.TypingCtx size) (Py ann)
exprToBloq CPL.VarE{var} = do
  ty <- Ctx.unsafeLookupE var
  pure $ PP.pretty "qlt_arith.Xor" <> PP.tupled [toQltDType ty]
exprToBloq CPL.UnOpE{un_op = CPL.NotOp, operand = CPL.VarE{}} = do
  pure $ PP.pretty "qlt_gates.XGate().controlled(qlt.CtrlSpec(cvs=0))"
exprToBloq CPL.BinOpE{bin_op = CPL.LEqOp, lhs = CPL.VarE{var}, rhs = CPL.ParamE{param}} = do
  ty <- Ctx.unsafeLookupE var
  let v = py_sanitizeIdent param
  pure $
    PP.pretty "qlt_arith.LessThanConstant"
      <> PP.tupled
        [ PP.pretty $ show $ CPL.bestBitsize ty
        , PP.parens (v <> PP.pretty " - 1")
        ]
exprToBloq CPL.UnOpE{un_op = CPL.AnyOp, operand = CPL.VarE{var}} = do
  ty <- Ctx.unsafeLookupE var
  pure $ namedBloq "AnyOp" [ty, CPL.tbool]
exprToBloq CPL.BinOpE{bin_op = CPL.VecSelectOp, lhs = CPL.VarE{var = x}, rhs = CPL.VarE{var = y}} = do
  tx <- Ctx.unsafeLookupE x
  let etx = case tx of
        CPL.Arr _ t -> t
        CPL.Bitvec _ -> CPL.tbool
        _ -> error "invalid type"
  ty <- Ctx.unsafeLookupE y
  pure $ namedBloq "VecSelectOp" [tx, ty, etx]
exprToBloq e = error $ "TODO Unitary embedding: " <> show e

instance (Show size, Integral size) => ToQualtranPy (QPL.BasicGate size) where
  type Ctx (QPL.BasicGate size) = [CPL.VarType size]

  -- simple gates
  mkPy QPL.Toffoli = pure $ PP.pretty "Toffoli()"
  mkPy QPL.CNOT = pure $ PP.pretty "qlt_gates.CNOT()"
  mkPy QPL.XGate = pure $ PP.pretty "qlt_gates.XGate()"
  mkPy QPL.HGate = pure $ PP.pretty "qlt_gates.Hadamard()"
  mkPy QPL.ZGate = pure $ PP.pretty "qlt_gates.ZGate()"
  mkPy (QPL.Rz theta) = pure $ PP.pretty @String $ printf "qlt_gates.Rz(%f)" theta
  -- generic gates
  mkPy QPL.COPY = do
    tys <- view id
    let n = length tys
    let half = n `div` 2
    let regs = zipWith py_register ["q_" <> show i | i <- [1 .. n]] tys
    pure $ PP.pretty "MultiCopy" <> PP.parens (PP.list regs)
  mkPy QPL.SWAP = do
    tys <- view id
    let n = length tys
    let half = n `div` 2
    let regs = zipWith py_register ["q_" <> show i | i <- [1 .. n]] tys
    pure $ PP.pretty "MultiSwap" <> PP.parens (PP.list regs)
  mkPy (QPL.PhaseOnZero theta) = do
    tys <- view id
    let n = length tys
    let regs = zipWith py_register ["q_" <> show i | i <- [1 .. n]] tys
    pure $ PP.pretty "PhaseOnZero" <> PP.tupled [PP.pretty theta, PP.list regs]

-- ============================================================
-- Classical: Emit native python
-- ============================================================

toPyType :: CPL.VarType size -> Py ann
toPyType (CPL.Fin _) = PP.pretty "int"
toPyType (CPL.Bitvec _) = PP.pretty "int"
toPyType (CPL.Tup ts) = PP.pretty "tuple" <+> PP.brackets (PP.tupled (map toPyType ts))
toPyType (CPL.Arr _ t) = PP.pretty "list" <+> PP.brackets (toPyType t)

instance (Show size) => ToQualtranPy (QPL.CProcBody size) where
  type Ctx (QPL.CProcBody size) = ProcBuildCtx size

  -- external
  mkPy QPL.CProcDecl = do
    ProcBuildCtx{..} <- view id
    let n_args = length proc_param_types
    let cproc_param_names = ["arg_" <> show i | i <- [1 .. n_args]]
    let tys = map toPyType proc_param_types

    let typed_args = map py_sanitizeIdent proc_meta_params ++ zipWith py_typedArg cproc_param_names tys
    pure $
      py_def proc_name typed_args $
        py_notImplemented "external function - implement here"

  -- defined
  mkPy QPL.CProcBody{cproc_param_names, cproc_local_vars, cproc_body_stmt} = do
    ProcBuildCtx{..} <- view id
    let tys = map toPyType proc_param_types

    let untyped_args = map py_sanitizeIdent (proc_meta_params ++ cproc_param_names)
    let typed_args = map py_sanitizeIdent proc_meta_params ++ zipWith py_typedArg cproc_param_names tys

    py_def proc_name typed_args . PP.vsep <$> do
      body <- withEnv () $ mkPy cproc_body_stmt
      return
        [ body
        , py_return untyped_args
        ]

instance (Show size) => ToQualtranPy (QPL.Stmt size) where
  type Ctx (QPL.Stmt size) = ()

  mkPy QPL.SkipS = pure py_pass
  mkPy (QPL.CommentS s) = pure $ py_comment s
  mkPy QPL.AssignS{rets, expr} = do
    let lhs = PP.hsep $ PP.punctuate PP.comma (map py_sanitizeIdent rets)
    pure $ lhs <+> PP.equals <+> py_expr expr
  mkPy QPL.RandomS{rets, distr_expr} = error "TODO RandomS"
  mkPy QPL.RandomDynS{ret, max_var} =
    pure $ PP.pretty ret <+> PP.equals <+> PP.pretty "random.randrange" <> PP.parens (PP.pretty max_var)
  mkPy QPL.CallS{fun = QPL.FunctionCall proc_id, meta_params, args} = do
    let fname = py_sanitizeIdent proc_id
    let py_mps = map py_metaParam meta_params
    let py_args = map py_arg args
    let all_args = py_mps ++ py_args
    let arg_vars = map py_arg args
    let lhs = PP.hsep $ PP.punctuate PP.comma arg_vars
    pure $ lhs <+> PP.equals <+> fname <> PP.tupled all_args
  mkPy QPL.CallS{fun = QPL.UProcAndMeas proc_id, meta_params, args} = do
    let py_mps = map py_metaParam meta_params
    let bloq = py_sanitizeIdent proc_id <> PP.tupled py_mps
    let py_args = map py_arg args
    let lhs = PP.hsep $ PP.punctuate PP.comma py_args
    pure $ lhs <+> PP.equals <+> PP.pretty "bloq_call_and_meas" <> PP.tupled (bloq : py_args)
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
  mkPy QPL.WhileK{n_iter, cond, loop_body} = error "TODO WhileK"
  mkPy QPL.WhileKWithCondExpr{n_iter, cond, cond_expr, loop_body} = error "TODO WhileKWithCondExpr"
  mkPy QPL.ForInArray{loop_index, loop_index_ty, loop_values, loop_body} = do
    body <- mkPy loop_body
    let vals = PP.list (map py_expr loop_values)
    pure $
      PP.vsep
        [ PP.pretty "for" <+> PP.pretty loop_index <+> PP.pretty "in" <+> vals <> PP.colon
        , py_indent body
        ]
  mkPy QPL.ForInRangeS{iter_meta_var, iter_lim, loop_body} = error "TODO ForInRangeS"
