module QCompose.ProtoLang.Syntax where

import qualified Data.Map as M
import QCompose.Basic

-- proto-search language
newtype VarType = Fin (Symbolic SizeT) -- Fin<N>
  deriving (Eq, Show, Read)

data UnOp = PNot
  deriving (Eq, Show, Read)

data BinOp = PAdd | PLeq | PAnd
  deriving (Eq, Show, Read)

data Stmt
  = SAssign {ret :: Ident, arg :: Ident}
  | SConst {ret :: Ident, val :: Value, ty :: VarType}
  | SUnOp {ret :: Ident, un_op :: UnOp, arg :: Ident}
  | SBinOp {ret :: Ident, bin_op :: BinOp, lhs :: Ident, rhs :: Ident}
  | SOracle {rets :: [Ident], args :: [Ident]}
  | SFunCall {rets :: [Ident], fun :: Ident, args :: [Ident]}
  | SIfTE {cond :: Ident, s_true :: Stmt, s_false :: Stmt}
  | SSeq Stmt Stmt -- S_1; S_2
  | SSearch {sol :: Ident, ok :: Ident, predicate :: Ident, args :: [Ident]}
  | SContains {ok :: Ident, predicate :: Ident, args :: [Ident]}
  deriving (Eq, Show, Read)

data FunDef = FunDef
  { name :: Ident
  , params :: [(Ident, VarType)]
  , rets :: [(Ident, VarType)]
  , body :: Stmt
  }
  deriving (Eq, Show, Read)

data OracleDef = OracleDef
  { paramTypes :: [VarType]
  , retTypes :: [VarType]
  }
  deriving (Eq, Show, Read)

data FunCtx = FunCtx
  { funs :: M.Map Ident FunDef
  , oracle :: OracleDef
  }
  deriving (Show)

data Program = Program
  { funCtx :: FunCtx
  , body :: Stmt
  }
