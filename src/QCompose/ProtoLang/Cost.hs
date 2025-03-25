module QCompose.ProtoLang.Cost (
  Complexity,
  QSearchFormulas (..),
  unitaryQueryCost,
  quantumQueryCost,
  needsEpsS,
  needsEpsP,
) where

import Control.Monad (filterM)
import Control.Monad.Reader (Reader, runReader)
import Control.Monad.State (execStateT)
import Data.Foldable (toList)
import Lens.Micro
import Lens.Micro.Mtl

import qualified QCompose.Utils.Context as Ctx
import QCompose.Utils.MonadHelpers
import qualified QCompose.Utils.Tree as Tree

import QCompose.Prelude
import QCompose.ProtoLang.Eval
import QCompose.ProtoLang.Syntax

-- | Computed cost functions (quantum, unitary) of a given set of algorithms implementing quantum search
data QSearchFormulas sizeT costT = QSearchFormulas
  { qSearchExpectedCost :: sizeT -> sizeT -> costT -> costT -- n t eps
  , qSearchWorstCaseCost :: sizeT -> costT -> costT -- n eps
  , qSearchUnitaryCost :: sizeT -> costT -> costT -- n delta
  }

detExtract :: Tree.Tree a -> a
detExtract xs = case toList xs of
  [x] -> x
  _ -> error "unexpected non-determinism"

unitaryQueryCost ::
  forall sizeT costT.
  (Num sizeT, Floating costT) =>
  -- | Qry formulas
  QSearchFormulas sizeT costT ->
  -- | precision (l2-norm)
  costT ->
  -- | program `P`
  Program sizeT ->
  costT
unitaryQueryCost algs d Program{funCtx, stmt} = cost d stmt
 where
  unsafeLookupFun :: Ident -> FunDef sizeT
  unsafeLookupFun fname =
    funCtx
      ^. to fun_defs
        . to (map $ \f -> (f ^. to fun_name, f))
        . atL fname

  costE :: costT -> Expr sizeT -> costT
  costE _ FunCallE{fun_kind = OracleCall} = 1
  -- TODO properly handle the funCtx
  costE delta FunCallE{fun_kind = FunctionCall fname} = 2 * cost (delta / 2) body
   where
    FunDef{body} = unsafeLookupFun fname
  costE delta FunCallE{fun_kind = PrimitiveCall c, args = (predicate : args)}
    | c == Contains || c == Search = qry * cost_pred
   where
    FunDef{param_binds} = unsafeLookupFun predicate
    Fin n = param_binds & last & snd

    -- split the precision
    delta_search = delta / 2
    delta_pred = delta - delta_search

    -- number of predicate queries
    qry = qSearchUnitaryCost algs n delta_search
    -- precision per predicate call
    delta_per_pred_call = delta_pred / qry

    cost_pred =
      costE delta_per_pred_call $
        FunCallE
          { fun_kind = FunctionCall predicate
          , args = args
          }
  -- zero-cost expressions
  costE _ VarE{} = 0
  costE _ ConstE{} = 0
  costE _ UnOpE{} = 0
  costE _ BinOpE{} = 0
  costE _ FunCallE{} = error "invalid syntax"

  cost :: costT -> Stmt sizeT -> costT
  cost delta ExprS{expr} = costE delta expr
  cost delta IfThenElseS{s_true, s_false} = cost delta s_true + cost delta s_false
  cost delta (SeqS [s]) = cost delta s
  cost delta (SeqS (s : ss)) = cost (delta / 2) s + cost (delta / 2) (SeqS ss)
  cost _ _ = error "invalid syntax"

quantumQueryCost ::
  -- | Qry formulas
  QSearchFormulas SizeT Complexity ->
  -- | failure probability `eps`
  FailProb ->
  -- | program `P`
  Program SizeT ->
  -- | oracle `O`
  OracleInterp ->
  -- | state `sigma`
  ProgramState ->
  Complexity
quantumQueryCost algs a_eps Program{funCtx, stmt} oracleF = cost a_eps stmt
 where
  unsafeLookupFun :: Ident -> FunDef SizeT
  unsafeLookupFun fname = head $ lookupFun fname funCtx

  get :: ProgramState -> Ident -> Value
  get = flip Ctx.get

  costE :: FailProb -> Expr SizeT -> ProgramState -> Complexity
  costE _ FunCallE{fun_kind = OracleCall} _ = 1
  costE eps FunCallE{fun_kind = FunctionCall f, args} sigma = cost eps body omega
   where
    vs = map (get sigma) args
    FunDef _ fn_args _ body = unsafeLookupFun f
    omega = Ctx.fromList $ zip (map fst fn_args) vs

  -- known cost formulas
  costE eps FunCallE{fun_kind = PrimitiveCall c, args = (predicate : args)} sigma
    | c == Contains || c == Search = n_pred_calls * pred_unitary_cost
   where
    vs = map (get sigma) args
    funDef@(FunDef _ fn_args _ body) = unsafeLookupFun predicate
    typ_x = snd $ last fn_args

    space = range typ_x
    sols = (`filterM` space) $ \v -> do
      result <- evalFun funCtx oracleF (vs ++ [v]) funDef
      let [b] = result
      return $ b /= 0

    n = length space
    t = minimum $ fmap length sols

    n_pred_calls = qSearchExpectedCost algs n t (eps / 2)

    q_worst = qSearchWorstCaseCost algs n (eps / 2)
    eps_per_pred_call = (eps / 2) / q_worst
    delta_per_pred_call = eps_per_pred_call / 2

    pred_unitary_cost = unitaryQueryCost algs delta_per_pred_call Program{funCtx, stmt = body}

  -- zero-cost expressions
  costE _ VarE{} _ = 0
  costE _ ConstE{} _ = 0
  costE _ UnOpE{} _ = 0
  costE _ BinOpE{} _ = 0
  -- unsupported
  costE _ FunCallE{} _ = error "invalid syntax"

  cost :: FailProb -> Stmt SizeT -> ProgramState -> Complexity
  cost eps ExprS{expr} sigma = costE eps expr sigma
  cost eps IfThenElseS{cond, s_true, s_false} sigma =
    let s = if get sigma cond /= 0 then s_true else s_false
     in cost eps s sigma
  cost eps (SeqS [s]) sigma = cost eps s sigma
  cost eps (SeqS (s : ss)) sigma = cost (eps / 2) s sigma + cost (eps / 2) (SeqS ss) sigma'
   where
    runner = execProgram Program{funCtx, stmt = s} oracleF
    sigma' = detExtract $ execStateT runner sigma
  -- all other statements
  cost _ _ _ = error "INVALID"

-- | Compute if a statement can fail and therefore needs a failure probability to implement
needsEpsS :: Stmt a -> Reader (FunCtx a) Bool
needsEpsS IfThenElseS{s_true, s_false} = (||) <$> needsEpsS s_true <*> needsEpsS s_false
needsEpsS (SeqS ss) = or <$> traverse needsEpsS ss
needsEpsS ExprS{expr = FunCallE{fun_kind = PrimitiveCall _}} = return True
needsEpsS ExprS{expr = FunCallE{fun_kind = FunctionCall f}} =
  view (to unsafeLookupFun . to body) >>= needsEpsS
 where
  unsafeLookupFun = head . lookupFun f
needsEpsS _ = return False

-- | Compute if a program can fail and therefore needs a failure probability to implement
needsEpsP :: Program a -> Bool
needsEpsP Program{funCtx, stmt} = runReader (needsEpsS stmt) funCtx
