module QCompose.BlockQPL where
	
import qualified Data.Map as M

-- cq-while language
data CQType = CType VarType | QType VarType

data CQWhile =
  -- simple
  Skip
  | Return [Ident]
  -- sequences
  | Seq [CQWhile]
  | Repeat Int CQWhile
  -- classical
  | CNew Ident
  | CAssign [Ident] SimpleExpr
  | CRandom Ident VarType
  | CDiscard Ident
  -- control flow
  | CWhile Ident CQWhile
  | CIfTE Ident CQWhile CQWhile
  -- q. unitary
  | QNew Ident VarType
  | QDiscard Ident
  | QUnitary [Ident] String
  -- quantum
  | QMeas Ident Ident
  -- placeholder
  | CQHole String
  deriving (Eq, Show, Read)

class Rewritable a where
  rewrite :: (a -> a) -> (a -> a)

instance Rewritable CQWhile where
  rewrite f (Seq ps) = f (Seq $ rewrite f <$> ps)
  rewrite f (Repeat n p) = f (Repeat n (f p))
  rewrite f (CWhile b p) = f (CWhile b (f p))
  rewrite f (CIfTE b p1 p0) = f (CIfTE b (f p1) (f p0))
  rewrite f p = f p

flatten_seq :: CQWhile -> CQWhile
flatten_seq = rewrite flatten_aux
  where
    flatten_aux :: CQWhile -> CQWhile
    flatten_aux (Seq ps) = 
      let ps' = concatMap into_seq ps in
      case ps' of
        [p] -> p
        _ -> Seq ps'
    flatten_aux p = p

    into_seq :: CQWhile -> [CQWhile]
    into_seq (Seq qs) = qs
    into_seq Skip = []
    into_seq q = [q]

-- Compilation
type TypeCtx = M.Map Ident VarType

type Compiler = TypeCtx -> Stmt -> CQWhile

-- 1. Classical Deterministic
compile_classical :: Compiler
-- compile_classical ctx (SReturn xs) = Return xs
-- compile_classical ctx (SLet xs expr body) =
--   Seq [ Seq [CNew x | x <- xs]
--       , compute_stmts
--       , compiled_body
--       ]
--   where
--     compiled_body = compile_classical ctx body

--     compute_stmts = 
--       case expr of 
--         E simple_expr -> CAssign xs simple_expr
--         ESearch pred args -> 
--           let [ans, ok] = xs in 
--             Seq [
--               CNew "loop",
--               CAssign ["loop"] (EConst 1 (Fin 1)),
--               CWhile "loop" $ Seq [
--                 CQHole "search_loop_body_here"
--               ]
--             ]
--         _ -> error $ "cannot compile: " <> show expr

compile_classical _ _ = error "not implemented"

-- 2. Classical Probabilistic
compile_prob :: Compiler
compile_prob _ _ = error "not implemented"

-- 3. Quantum Unitary
compile_unitary :: Compiler
compile_unitary _ _ = error "not implemented"

-- 4. Quantum (with measurements)
compile_quantum :: Compiler
compile_quantum _ _ = error "not implemented"
