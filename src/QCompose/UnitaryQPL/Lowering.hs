module QCompose.UnitaryQPL.Lowering where

import Control.Monad.State (StateT, runStateT)
import Control.Monad.Trans (lift)
import Lens.Micro
import Lens.Micro.Mtl
import QCompose.Basic
import QCompose.ProtoLang.Syntax
import QCompose.ProtoLang.TypeCheck
import QCompose.UnitaryQPL.Syntax
import QCompose.Utils.Context

{- | A global lowering context storing the source functions and generated procedures
 along with info to generate unique ancilla and variable/procedure names
-}
data LoweringCtx a = LoweringCtx
  { protoFunCtx :: FunCtx a
  , uniqNameIdx :: Int
  , ancillaIdx :: Int
  , uqplProcs :: [UQPLProcDef a] -- generated procedures (stored in reverse order)
  , typingCtx :: TypingCtx a
  }

makeLoweringCtx :: FunCtx a -> TypingCtx a -> LoweringCtx a
makeLoweringCtx funCtx gamma =
  LoweringCtx
    { protoFunCtx = funCtx
    , uniqNameIdx = 0
    , ancillaIdx = 0
    , uqplProcs = []
    , typingCtx = gamma
    }

_typingCtx :: Lens' (LoweringCtx a) (TypingCtx a)
_typingCtx = lens typingCtx (\ctx g -> ctx{typingCtx = g})

_protoFunCtx :: Lens' (LoweringCtx a) (FunCtx a)
_protoFunCtx = lens protoFunCtx (\ctx f -> ctx{protoFunCtx = f})

_ancillaIdx :: Lens' (LoweringCtx a) Int
_ancillaIdx = lens ancillaIdx (\ctx v -> ctx{ancillaIdx = v})

_uniqNameIdx :: Lens' (LoweringCtx a) Int
_uniqNameIdx = lens uniqNameIdx (\ctx v -> ctx{uniqNameIdx = v})

_uqplProcs :: Lens' (LoweringCtx a) [UQPLProcDef a]
_uqplProcs = lens uqplProcs (\ctx v -> ctx{uqplProcs = v})

{- | The nested state monad to compile ProtoQB to UQPL programs.
This should contain the _final_ typing context for the input program,
that is, contains both the inputs and outputs of each statement.
-}
type UQPLCompilerT a = StateT (LoweringCtx a) (Either String)

-- | Allocate an ancilla register, and update the typing context
allocAncilla :: VarType a -> UQPLCompilerT a Ident
allocAncilla ty = do
  i <- use _ancillaIdx
  _ancillaIdx += 1

  let name = "_a_" <> show i
  zoom _typingCtx $ putValue name ty
  return name

-- | A simple name-mangling utility to generate a unique proc name
newProcName :: Ident -> UQPLCompilerT a Ident
newProcName name = do
  i <- use _uniqNameIdx
  _uniqNameIdx += 1
  return $ "_" <> name <> "_" <> show i

lowerU :: (TypeCheckable a) => Precision -> Stmt a -> UQPLCompilerT a (UQPLStmt a)
-- basic statements (do not depend on precision)
lowerU _ AssignS{arg, ret} = do
  ty <- zoom _typingCtx $ lookupVar arg
  return $ UnitaryU{args = [arg, ret], unitary = RevEmbed (IdF ty)}
lowerU _ ConstS{ret, val, ty} = do
  return $ UnitaryU{args = [ret], unitary = RevEmbed (ConstF ty val)}
lowerU _ UnOpS{un_op = NotOp, ret, arg} = do
  ty <- zoom _typingCtx $ lookupVar arg
  return $ UnitaryU{args = [arg, ret], unitary = RevEmbed (NotF ty)}
lowerU _ BinOpS{bin_op, ret, lhs, rhs} = do
  ty <- zoom _typingCtx $ lookupVar lhs
  let unitary = case bin_op of
        AddOp -> RevEmbed (AddF ty)
        LEqOp -> RevEmbed (LEqF ty)
        AndOp -> Toffoli
  return $ UnitaryU{args = [lhs, rhs, ret], unitary}
lowerU _ FunCallS{fun_kind = OracleCall, args, rets} = do
  return $ UnitaryU{args = args ++ rets, unitary = Oracle}
-- function/subroutine calls
lowerU delta FunCallS{fun_kind = FunctionCall fname, args, rets} = do
  fun_def <- use _protoFunCtx >>= (`lookupFun` fname)
  UQPLProcDef{proc_name, proc_params} <- lowerProcU delta fun_def
  let proc_args = undefined
  return $ CallU{proc_name, args = proc_args}
lowerU _ FunCallS{fun_kind = SubroutineCall Contains, args, rets} = do
  error "TODO"
-- composite statements
lowerU _ (SeqS []) = return SkipU
lowerU delta (SeqS [s]) = lowerU delta s
lowerU delta (SeqS (s : ss)) = do
  s' <- lowerU (delta / 2) s
  ss' <- lowerU (delta / 2) (SeqS ss)
  return $ SeqU [s', ss']
-- currently unsupported syntax
lowerU _ _ = error "uqpl lowering: unsuppored operation!"

{- | lower a function def to a uproc def.
 return the generated function
-}
lowerProcU :: (TypeCheckable a) => Precision -> FunDef a -> UQPLCompilerT a (UQPLProcDef a)
lowerProcU delta funDef = do
  gamma_save <- use _typingCtx
  funCtx <- use _protoFunCtx
  omega <- lift $ typeCheckFun funCtx funDef
  _typingCtx .= omega
  undefined

{- | Lower a program given a precision and input typing context.
 Produces a UQPL program and a full typing context.
 The produced typing context can be partitioned into three: input, output and auxillary.

 TODO returned the partitoned register spaces.
-}
lowerProgramU ::
  (TypeCheckable a) =>
  TypingCtx a ->
  Precision ->
  Program a ->
  Either String (UQPLProgram a, TypingCtx a)
lowerProgramU gamma delta p@Program{funCtx, stmt} = do
  -- get the final typing context (i.e. including outputs)
  gamma' <- typeCheckProg gamma p

  -- use the full typing context to compile the program.
  let loweringCtx = makeLoweringCtx funCtx gamma'
  (stmtU, loweringCtx') <- runStateT (lowerU delta stmt) loweringCtx

  return
    ( UQPLProgram
        { oracleU = oracle funCtx
        , stmtU = stmtU
        , procs = loweringCtx' ^. _uqplProcs & reverse
        }
    , loweringCtx' ^. _typingCtx
    )
