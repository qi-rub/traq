module QCompose.UnitaryQPL.Lowering where

import Control.Monad.State (StateT, runStateT)
import Control.Monad.Trans (lift)
import Lens.Micro
import Lens.Micro.Mtl
import QCompose.Basic
import qualified QCompose.ProtoLang as P
import QCompose.UnitaryQPL.Syntax
import QCompose.Utils.Context

{- | A global lowering context storing the source functions and generated procedures
 along with info to generate unique ancilla and variable/procedure names
-}
data LoweringCtx a = LoweringCtx
  { protoFunCtx :: P.FunCtx a
  , uniqNameIdx :: Int
  , ancillaIdx :: Int
  , procDefs :: [ProcDef a] -- generated procedures (stored in reverse order)
  , typingCtx :: P.TypingCtx a
  }

makeLoweringCtx :: P.FunCtx a -> P.TypingCtx a -> LoweringCtx a
makeLoweringCtx funCtx gamma =
  LoweringCtx
    { protoFunCtx = funCtx
    , uniqNameIdx = 0
    , ancillaIdx = 0
    , procDefs = []
    , typingCtx = gamma
    }

_typingCtx :: Lens' (LoweringCtx a) (P.TypingCtx a)
_typingCtx = lens typingCtx (\ctx g -> ctx{typingCtx = g})

_protoFunCtx :: Lens' (LoweringCtx a) (P.FunCtx a)
_protoFunCtx = lens protoFunCtx (\ctx f -> ctx{protoFunCtx = f})

_ancillaIdx :: Lens' (LoweringCtx a) Int
_ancillaIdx = lens ancillaIdx (\ctx v -> ctx{ancillaIdx = v})

_uniqNameIdx :: Lens' (LoweringCtx a) Int
_uniqNameIdx = lens uniqNameIdx (\ctx v -> ctx{uniqNameIdx = v})

_procDefs :: Lens' (LoweringCtx a) [ProcDef a]
_procDefs = lens procDefs (\ctx v -> ctx{procDefs = v})

{- | The nested state monad to compile ProtoQB to  programs.
This should contain the _final_ typing context for the input program,
that is, contains both the inputs and outputs of each statement.
-}
type CompilerT a = StateT (LoweringCtx a) (Either String)

-- | Allocate an ancilla register, and update the typing context
allocAncilla :: P.VarType a -> CompilerT a Ident
allocAncilla ty = do
  i <- use _ancillaIdx
  _ancillaIdx += 1

  let name = "_a_" <> show i
  zoom _typingCtx $ putValue name ty
  return name

-- | A simple name-mangling utility to generate a unique proc name
newProcName :: Ident -> CompilerT a Ident
newProcName name = do
  i <- use _uniqNameIdx
  _uniqNameIdx += 1
  return $ "_" <> name <> "_" <> show i

lowerU :: (P.TypeCheckable a) => Precision -> P.Stmt a -> CompilerT a (Stmt a)
-- basic statements (do not depend on precision)
lowerU _ P.AssignS{arg, ret} = do
  ty <- zoom _typingCtx $ lookupVar arg
  return $ UnitaryU{args = [arg, ret], unitary = RevEmbed (IdF ty)}
lowerU _ P.ConstS{ret, val, ty} = do
  return $ UnitaryU{args = [ret], unitary = RevEmbed (ConstF ty val)}
lowerU _ P.UnOpS{un_op = P.NotOp, ret, arg} = do
  ty <- zoom _typingCtx $ lookupVar arg
  return $ UnitaryU{args = [arg, ret], unitary = RevEmbed (NotF ty)}
lowerU _ P.BinOpS{bin_op, ret, lhs, rhs} = do
  ty <- zoom _typingCtx $ lookupVar lhs
  let unitary = case bin_op of
        P.AddOp -> RevEmbed (AddF ty)
        P.LEqOp -> RevEmbed (LEqF ty)
        P.AndOp -> Toffoli
  return $ UnitaryU{args = [lhs, rhs, ret], unitary}
lowerU _ P.FunCallS{fun_kind = P.OracleCall, args, rets} = do
  return $ UnitaryU{args = args ++ rets, unitary = Oracle}
-- function/subroutine calls
lowerU delta P.FunCallS{fun_kind = P.FunctionCall fname, args, rets} = do
  fun_def <- use _protoFunCtx >>= (`P.lookupFun` fname)
  ProcDef{proc_name, proc_params} <- lowerProcU delta fun_def
  let proc_args = undefined
  return $ CallU{proc_name, args = proc_args}
lowerU _ P.FunCallS{fun_kind = P.SubroutineCall P.Contains, args, rets} = do
  error "TODO"
-- composite statements
lowerU _ (P.SeqS []) = return SkipU
lowerU delta (P.SeqS [s]) = lowerU delta s
lowerU delta (P.SeqS (s : ss)) = do
  s' <- lowerU (delta / 2) s
  ss' <- lowerU (delta / 2) (P.SeqS ss)
  return $ SeqU [s', ss']
-- currently unsupported syntax
lowerU _ _ = error " lowering: unsuppored operation!"

{- | lower a function def to a uproc def.
 return the generated function
-}
lowerProcU :: (P.TypeCheckable a) => Precision -> P.FunDef a -> CompilerT a (ProcDef a)
lowerProcU delta funDef = do
  gamma_save <- use _typingCtx
  funCtx <- use _protoFunCtx
  omega <- lift $ P.typeCheckFun funCtx funDef
  _typingCtx .= omega
  undefined

{- | Lower a program given a precision and input typing context.
 Produces a  program and a full typing context.
 The produced typing context can be partitioned into three: input, output and auxillary.

 TODO returned the partitoned register spaces.
-}
lowerProgramU ::
  (P.TypeCheckable a) =>
  P.TypingCtx a ->
  Precision ->
  P.Program a ->
  Either String (Program a, P.TypingCtx a)
lowerProgramU gamma delta p@P.Program{funCtx, stmt} = do
  -- get the final typing context (i.e. including outputs)
  gamma' <- P.typeCheckProg gamma p

  -- use the full typing context to compile the program.
  let loweringCtx = makeLoweringCtx funCtx gamma'
  (stmtU, loweringCtx') <- runStateT (lowerU delta stmt) loweringCtx

  return
    ( Program
        { oracleU = P.oracle funCtx
        , stmtU = stmtU
        , procs = loweringCtx' ^. _procDefs & reverse
        }
    , loweringCtx' ^. _typingCtx
    )
