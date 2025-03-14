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

lower :: (P.TypeCheckable a) => Precision -> P.Stmt a -> CompilerT a (Stmt a)
-- basic statements (do not depend on precision)
lower _ P.AssignS{P.arg, P.ret} = do
  ty <- zoom _typingCtx $ lookupVar arg
  return $ UnitaryU{args = [arg, ret], unitary = RevEmbed (IdF ty)}
lower _ P.ConstS{P.ret, P.val, P.ty} = do
  return $ UnitaryU{args = [ret], unitary = RevEmbed (ConstF ty val)}
lower _ P.UnOpS{P.un_op = P.NotOp, P.ret, P.arg} = do
  ty <- zoom _typingCtx $ lookupVar arg
  return $ UnitaryU{args = [arg, ret], unitary = RevEmbed (NotF ty)}
lower _ P.BinOpS{P.bin_op, P.ret, P.lhs, P.rhs} = do
  ty <- zoom _typingCtx $ lookupVar lhs
  let unitary = case bin_op of
        P.AddOp -> RevEmbed (AddF ty)
        P.LEqOp -> RevEmbed (LEqF ty)
        P.AndOp -> Toffoli
  return $ UnitaryU{args = [lhs, rhs, ret], unitary}
lower _ P.FunCallS{P.fun_kind = P.OracleCall, P.args, P.rets} = do
  return $ UnitaryU{args = args ++ rets, unitary = Oracle}
-- function/subroutine calls
lower delta P.FunCallS{P.fun_kind = P.FunctionCall fname, P.args, P.rets} = do
  fun_def <- use _protoFunCtx >>= (`P.lookupFun` fname)
  ProcDef{proc_name, proc_params} <- lowerProc delta fun_def
  let proc_args = undefined
  return $ CallU{proc_id = proc_name, args = proc_args}
lower _ P.FunCallS{P.fun_kind = P.SubroutineCall P.Contains, P.args, P.rets} = do
  error "TODO"
-- composite statements
lower _ (P.SeqS []) = return SkipU
lower delta (P.SeqS [s]) = lower delta s
lower delta (P.SeqS (s : ss)) = do
  s' <- lower (delta / 2) s
  ss' <- lower (delta / 2) (P.SeqS ss)
  return $ SeqU [s', ss']
-- currently unsupported syntax
lower _ _ = error " lowering: unsuppored operation!"

{- | lower a function def to a uproc def.
 return the generated function
-}
lowerProc :: (P.TypeCheckable a) => Precision -> P.FunDef a -> CompilerT a (ProcDef a)
lowerProc delta funDef = do
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
lowerProgram ::
  (P.TypeCheckable a) =>
  P.TypingCtx a ->
  Precision ->
  P.Program a ->
  Either String (Program a, P.TypingCtx a)
lowerProgram gamma delta p@P.Program{P.funCtx, P.stmt} = do
  -- get the final typing context (i.e. including outputs)
  gamma' <- P.typeCheckProg gamma p

  -- use the full typing context to compile the program.
  let loweringCtx = makeLoweringCtx funCtx gamma'
  (stmtU, loweringCtx') <- runStateT (lower delta stmt) loweringCtx

  return
    ( Program
        { oracle_decl = P.oracle funCtx
        , stmt = stmtU
        , proc_defs = loweringCtx' ^. _procDefs & reverse
        }
    , loweringCtx' ^. _typingCtx
    )
