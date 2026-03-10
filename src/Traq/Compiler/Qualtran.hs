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

import qualified Traq.CQPL as CQPL
import Traq.Prelude
import qualified Traq.ProtoLang as P

-- ============================================================
-- Compile QPL -> py (+Qualtran)
-- ============================================================

type Py ann = PP.Doc ann

-- | Build python code string.
class ToQualtranPy a where
  type Ctx a

  mkPy :: a -> Reader (Ctx a) (Py ann)

-- | Convert a CQPL program to a python code string.
toPy :: CQPL.Program SizeT -> String
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
toQltDType :: (Show size, Integral size) => P.VarType size -> Py ann
toQltDType (P.Fin n) =
  PP.pretty "qlt.BQUInt" <> PP.tupled [PP.pretty (show $ bitsize n), PP.pretty (show n)]
toQltDType (P.Bitvec n) = PP.pretty "qlt.QAny" <> PP.parens (PP.pretty (show n))
toQltDType (P.Tup _) = error "TODO toQltDType Tup"
toQltDType (P.Arr _ t) = toQltDType t -- base dtype; shape handled in py_register

-- | Build shape tuple for a VarType (only Arr adds dimensions)
toQltShape :: (Show size) => P.VarType size -> [Py ann]
toQltShape (P.Arr n t) = PP.pretty (show n) : toQltShape t
toQltShape _ = []

-- | Build a qlt.Register(...) expression
py_register :: (Show size, Integral size) => Ident -> P.VarType size -> Py ann
py_register name ty =
  let dtype = toQltDType ty
      shape = toQltShape ty
      shapeArg = case shape of
        [] -> []
        _ -> [PP.pretty "shape=" <> PP.tupled shape]
   in PP.pretty "qlt.Register" <> PP.tupled ([PP.dquotes (py_sanitizeIdent name), dtype] ++ shapeArg)

py_metaParam :: (Show size) => Either (P.MetaParam size) Ident -> Py ann
py_metaParam (Left (P.MetaName n)) = py_sanitizeIdent n
py_metaParam (Left (P.MetaSize s)) = PP.pretty (show s)
py_metaParam (Right name) = py_sanitizeIdent name

py_arg :: (Show size) => CQPL.Arg size -> Py ann
py_arg (CQPL.Arg x) = py_sanitizeIdent x
py_arg (CQPL.ArrElemArg a i) = py_arg a <> PP.brackets (py_metaParam (Left i))

-- | Emit a python expression
py_expr :: (Show size) => P.BasicExpr size -> Py ann
py_expr P.VarE{var} = py_sanitizeIdent var
py_expr P.ParamE{param} = py_sanitizeIdent param
py_expr P.DefaultE{ty} = py_defaultVal ty
py_expr P.ConstE{val} = py_val val
py_expr P.UnOpE{un_op, operand} = py_unOp un_op <> PP.parens (py_expr operand)
py_expr P.BinOpE{bin_op, lhs, rhs} = PP.parens $ py_expr lhs <+> py_binOp bin_op <+> py_expr rhs
py_expr P.TernaryE{branch, lhs, rhs} = PP.parens $ py_expr lhs <+> PP.pretty "if" <+> py_expr branch <+> PP.pretty "else" <+> py_expr rhs
py_expr P.NAryE{op, operands} = py_naryOp op <> PP.tupled (map py_expr operands)
py_expr P.IndexE{arr_expr, ix_val} = py_expr arr_expr <> PP.brackets (PP.pretty (show ix_val))
py_expr P.DynIndexE{arr_expr, ix_expr} = py_expr arr_expr <> PP.brackets (py_expr ix_expr)
py_expr P.UpdateArrE{arr_expr, ix_expr, rhs} = error "TODO UpdateArrE"
py_expr P.ProjectE{tup_expr, tup_ix_val} = py_expr tup_expr <> PP.brackets (PP.pretty (show tup_ix_val))

py_val :: (Show size) => P.Value size -> Py ann
py_val (P.FinV n) = PP.pretty (show n)
py_val (P.ArrV vs) = PP.list (map py_val vs)
py_val (P.TupV vs) = PP.tupled (map py_val vs)

py_defaultVal :: (Show size) => P.VarType size -> Py ann
py_defaultVal (P.Fin _) = PP.pretty "0"
py_defaultVal (P.Bitvec _) = PP.pretty "0"
py_defaultVal (P.Arr n t) = PP.brackets (py_defaultVal t) <+> PP.pretty "*" <+> PP.pretty (show n)
py_defaultVal (P.Tup ts) = PP.tupled (map py_defaultVal ts)

py_unOp :: P.UnOp -> Py ann
py_unOp P.NotOp = PP.pretty "not "
py_unOp P.AnyOp = PP.pretty "any"
py_unOp P.AllOp = PP.pretty "all"
py_unOp P.MajOp = error "TODO MajOp"

py_binOp :: P.BinOp -> Py ann
py_binOp P.AddOp = PP.pretty "+"
py_binOp P.MulOp = PP.pretty "*"
py_binOp P.SubOp = PP.pretty "-"
py_binOp P.XorOp = PP.pretty "^"
py_binOp P.LEqOp = PP.pretty "<="
py_binOp P.LtOp = PP.pretty "<"
py_binOp P.AndOp = PP.pretty "and"
py_binOp P.EqOp = PP.pretty "=="
py_binOp P.VecSelectOp = error "TODO VecSelectOp"

py_naryOp :: P.NAryOp -> Py ann
py_naryOp P.MultiOrOp = PP.pretty "any"

-- ============================================================
-- Basic Instances
-- ============================================================

instance (Show size, Integral size) => ToQualtranPy (CQPL.Program size) where
  type Ctx (CQPL.Program size) = ()

  mkPy (CQPL.Program ps) =
    PP.vsep . intersperse PP.line <$> mapM mkPy ps

instance (Show size, Integral size) => ToQualtranPy (CQPL.ProcDef size) where
  type Ctx (CQPL.ProcDef size) = ()

  mkPy CQPL.ProcDef{info_comment, proc_name, proc_meta_params, proc_param_types, proc_body} =
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
  , proc_param_types :: [P.VarType size]
  }
  deriving (Read, Show, Eq)

instance (Show size, Integral size) => ToQualtranPy (CQPL.ProcBody size) where
  type Ctx (CQPL.ProcBody size) = ProcBuildCtx size

  mkPy (CQPL.ProcBodyU ubody) = mkPy ubody
  mkPy (CQPL.ProcBodyC cbody) = mkPy cbody

-- ============================================================
-- Unitary: Emit Qualtran Bloqs
-- ============================================================

instance (Show size, Integral size) => ToQualtranPy (CQPL.UProcBody size) where
  type Ctx (CQPL.UProcBody size) = ProcBuildCtx size

  mkPy CQPL.UProcDecl = do
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
  mkPy CQPL.UProcBody{uproc_param_names, uproc_param_tags, uproc_body_stmt} = do
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

instance (Show size, Integral size) => ToQualtranPy (CQPL.UStmt size) where
  type Ctx (CQPL.UStmt size) = P.TypingCtx size

  mkPy CQPL.USkipS = pure mempty
  mkPy (CQPL.UCommentS s) = pure $ py_comment s
  mkPy CQPL.UnitaryS{qargs, unitary} = do
    tys <- fmap (either (error . show) id) . runExceptT $ do
      mapM CQPL.getArgTy qargs
    bloqExpr <- withEnv tys $ mkPy unitary
    let argVals = PP.list [py_arg q | q <- qargs]
    let lhs = PP.hsep $ PP.punctuate PP.comma [py_arg q | q <- qargs]
    pure $ lhs <+> PP.equals <+> PP.pretty "add_bloq" <> PP.tupled [PP.pretty "bb", bloqExpr, argVals]
  mkPy CQPL.UCallS{uproc_id, dagger, qargs} = do
    let bloq = py_sanitizeIdent uproc_id <> PP.pretty "()"
    let bloqExpr = if dagger then bloq <> PP.pretty ".adjoint()" else bloq
    let argVals = PP.list [py_arg q | q <- qargs]
    let lhs = PP.hsep $ PP.punctuate PP.comma [py_arg q | q <- qargs]
    pure $ lhs <+> PP.equals <+> PP.pretty "add_bloq" <> PP.tupled [PP.pretty "bb", bloqExpr, argVals]
  -- compound statements
  mkPy (CQPL.USeqS ss) = PP.vsep <$> mapM mkPy ss
  mkPy CQPL.URepeatS{n_iter, uloop_body} = do
    body <- mkPy uloop_body
    let n = py_metaParam (Left n_iter)
    pure $
      PP.vsep
        [ PP.pretty "for _ in range" <> PP.parens n <> PP.colon
        , py_indent body
        ]
  mkPy CQPL.UForInRangeS{iter_meta_var, iter_lim, dagger, uloop_body} = do
    body <- mkPy (if dagger then CQPL.adjoint uloop_body else uloop_body)
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
  mkPy CQPL.UForInDomainS{iter_meta_var, iter_ty, dagger, uloop_body} = pure $ py_notImplemented "TODO UForInDomainS"
  mkPy CQPL.UWithComputedS{with_ustmt, body_ustmt} = do
    mkPy with_ustmt
    mkPy body_ustmt
    mkPy (CQPL.adjoint with_ustmt)

instance (Show size, Integral size) => ToQualtranPy (CQPL.Unitary size) where
  type Ctx (CQPL.Unitary size) = [P.VarType size]

  mkPy (CQPL.BasicGateU g) = mkPy g
  mkPy (CQPL.DistrU (P.UniformE ty)) = do
    let bs = P.bestBitsize ty
    pure $ PP.pretty "QFTTextBook" <> PP.tupled [PP.pretty (show bs)]
  mkPy (CQPL.DistrU (P.BernoulliE p)) = do
    let theta = PP.pretty @String $ printf "%f" (2 * asin (sqrt p))
    pure $ PP.pretty "qlt_gates.Ry" <> PP.tupled [PP.pretty "angle=" <> theta]
  mkPy (CQPL.Controlled u) = do
    bloq <- mkPy u
    pure $ bloq <> PP.pretty ".controlled()"
  mkPy (CQPL.Adjoint u) = do
    bloq <- mkPy u
    pure $ bloq <> PP.pretty ".adjoint()"

  -- embed classical gates as unitaries
  mkPy (CQPL.RevEmbedU xs e) = do
    tys <- view id
    let ctx = Ctx.fromList $ zip xs tys
    withEnv ctx $ exprToBloq e

namedBloq :: (Show size, Integral size) => Ident -> [P.VarType size] -> Py ann
namedBloq b ts =
  PP.pretty "NamedBloq"
    <> PP.tupled
      [ PP.dquotes $ PP.pretty b
      , PP.list [py_register ("x_" <> show i) t | (t, i) <- zip ts [0 ..]]
      ]

exprToBloq :: (Show size, Integral size) => P.BasicExpr size -> Reader (P.TypingCtx size) (Py ann)
exprToBloq P.VarE{var} = do
  ty <- Ctx.unsafeLookupE var
  pure $ PP.pretty "qlt_arith.Xor" <> PP.tupled [toQltDType ty]
exprToBloq P.UnOpE{un_op = P.NotOp, operand = P.VarE{}} = do
  pure $ PP.pretty "qlt_gates.XGate().controlled(qlt.CtrlSpec(cvs=0))"
exprToBloq P.BinOpE{bin_op = P.LEqOp, lhs = P.VarE{var}, rhs = P.ParamE{param}} = do
  ty <- Ctx.unsafeLookupE var
  let v = py_sanitizeIdent param
  pure $
    PP.pretty "qlt_arith.LessThanConstant"
      <> PP.tupled
        [ PP.pretty $ show $ P.bestBitsize ty
        , PP.parens (v <> PP.pretty " - 1")
        ]
exprToBloq P.UnOpE{un_op = P.AnyOp, operand = P.VarE{var}} = do
  ty <- Ctx.unsafeLookupE var
  pure $ namedBloq "AnyOp" [ty, P.tbool]
exprToBloq P.BinOpE{bin_op = P.VecSelectOp, lhs = P.VarE{var = x}, rhs = P.VarE{var = y}} = do
  tx <- Ctx.unsafeLookupE x
  let etx = case tx of
        P.Arr _ t -> t
        P.Bitvec _ -> P.tbool
        _ -> error "invalid type"
  ty <- Ctx.unsafeLookupE y
  pure $ namedBloq "VecSelectOp" [tx, ty, etx]
exprToBloq e = error $ "TODO Unitary embedding: " <> show e

instance (Show size, Integral size) => ToQualtranPy (CQPL.BasicGate size) where
  type Ctx (CQPL.BasicGate size) = [P.VarType size]

  -- simple gates
  mkPy CQPL.Toffoli = pure $ PP.pretty "Toffoli()"
  mkPy CQPL.CNOT = pure $ PP.pretty "qlt_gates.CNOT()"
  mkPy CQPL.XGate = pure $ PP.pretty "qlt_gates.XGate()"
  mkPy CQPL.HGate = pure $ PP.pretty "qlt_gates.Hadamard()"
  mkPy CQPL.ZGate = pure $ PP.pretty "qlt_gates.ZGate()"
  mkPy (CQPL.Rz theta) = pure $ PP.pretty @String $ printf "qlt_gates.Rz(%f)" theta
  -- generic gates
  mkPy CQPL.COPY = do
    tys <- view id
    let n = length tys
    let half = n `div` 2
    let regs = zipWith py_register ["q_" <> show i | i <- [1 .. n]] tys
    pure $ PP.pretty "MultiCopy" <> PP.parens (PP.list regs)
  mkPy CQPL.SWAP = do
    tys <- view id
    let n = length tys
    let half = n `div` 2
    let regs = zipWith py_register ["q_" <> show i | i <- [1 .. n]] tys
    pure $ PP.pretty "MultiSwap" <> PP.parens (PP.list regs)
  mkPy (CQPL.PhaseOnZero theta) = do
    tys <- view id
    let n = length tys
    let regs = zipWith py_register ["q_" <> show i | i <- [1 .. n]] tys
    pure $ PP.pretty "PhaseOnZero" <> PP.tupled [PP.pretty theta, PP.list regs]

-- ============================================================
-- Classical: Emit native python
-- ============================================================

toPyType :: P.VarType size -> Py ann
toPyType (P.Fin _) = PP.pretty "int"
toPyType (P.Bitvec _) = PP.pretty "int"
toPyType (P.Tup ts) = PP.pretty "tuple" <+> PP.brackets (PP.tupled (map toPyType ts))
toPyType (P.Arr _ t) = PP.pretty "list" <+> PP.brackets (toPyType t)

instance (Show size) => ToQualtranPy (CQPL.CProcBody size) where
  type Ctx (CQPL.CProcBody size) = ProcBuildCtx size

  -- external
  mkPy CQPL.CProcDecl = do
    ProcBuildCtx{..} <- view id
    let n_args = length proc_param_types
    let cproc_param_names = ["arg_" <> show i | i <- [1 .. n_args]]
    let tys = map toPyType proc_param_types

    let typed_args = map py_sanitizeIdent proc_meta_params ++ zipWith py_typedArg cproc_param_names tys
    pure $
      py_def proc_name typed_args $
        py_notImplemented "external function - implement here"

  -- defined
  mkPy CQPL.CProcBody{cproc_param_names, cproc_local_vars, cproc_body_stmt} = do
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

instance (Show size) => ToQualtranPy (CQPL.Stmt size) where
  type Ctx (CQPL.Stmt size) = ()

  mkPy CQPL.SkipS = pure py_pass
  mkPy (CQPL.CommentS s) = pure $ py_comment s
  mkPy CQPL.AssignS{rets, expr} = do
    let lhs = PP.hsep $ PP.punctuate PP.comma (map py_sanitizeIdent rets)
    pure $ lhs <+> PP.equals <+> py_expr expr
  mkPy CQPL.RandomS{rets, distr_expr} = error "TODO RandomS"
  mkPy CQPL.RandomDynS{ret, max_var} =
    pure $ PP.pretty ret <+> PP.equals <+> PP.pretty "random.randrange" <> PP.parens (PP.pretty max_var)
  mkPy CQPL.CallS{fun = CQPL.FunctionCall proc_id, meta_params, args} = do
    let fname = py_sanitizeIdent proc_id
    let py_mps = map py_metaParam meta_params
    let py_args = map py_arg args
    let all_args = py_mps ++ py_args
    let arg_vars = map py_arg args
    let lhs = PP.hsep $ PP.punctuate PP.comma arg_vars
    pure $ lhs <+> PP.equals <+> fname <> PP.tupled all_args
  mkPy CQPL.CallS{fun = CQPL.UProcAndMeas proc_id, meta_params, args} = do
    let py_mps = map py_metaParam meta_params
    let bloq = py_sanitizeIdent proc_id <> PP.tupled py_mps
    let py_args = map py_arg args
    let lhs = PP.hsep $ PP.punctuate PP.comma py_args
    pure $ lhs <+> PP.equals <+> PP.pretty "bloq_call_and_meas" <> PP.tupled (bloq : py_args)
  mkPy (CQPL.SeqS ss) = PP.vsep <$> mapM mkPy ss
  mkPy CQPL.IfThenElseS{cond, s_true, s_false} = py_ifte cond <$> mkPy s_true <*> mkPy s_false
  mkPy CQPL.RepeatS{n_iter, loop_body} = do
    body <- mkPy loop_body
    let n = py_metaParam (Left n_iter)
    pure $
      PP.vsep
        [ PP.pretty "for _ in range" <> PP.parens n <> PP.colon
        , py_indent body
        ]
  mkPy CQPL.WhileK{n_iter, cond, loop_body} = error "TODO WhileK"
  mkPy CQPL.WhileKWithCondExpr{n_iter, cond, cond_expr, loop_body} = error "TODO WhileKWithCondExpr"
  mkPy CQPL.ForInArray{loop_index, loop_index_ty, loop_values, loop_body} = do
    body <- mkPy loop_body
    let vals = PP.list (map py_expr loop_values)
    pure $
      PP.vsep
        [ PP.pretty "for" <+> PP.pretty loop_index <+> PP.pretty "in" <+> vals <> PP.colon
        , py_indent body
        ]
  mkPy CQPL.ForInRangeS{iter_meta_var, iter_lim, loop_body} = error "TODO ForInRangeS"
