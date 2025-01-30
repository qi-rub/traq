module QCompose.ProtoLang.Syntax where

import qualified Data.Map as M

-- proto-search language
newtype VarType = Fin Int -- Fin<N>
  deriving (Eq, Show, Read)

type Ident = String

data UnOp = PNot
  deriving (Eq, Show, Read)

data BinOp = PAdd | PLeq | PAnd
  deriving (Eq, Show, Read)

data Stmt
  = SAssign Ident Ident -- x... <- y...
  | SConst Ident Int VarType -- x <- v :: T
  | SUnOp Ident UnOp Ident -- x <- op y
  | SBinOp Ident BinOp Ident Ident -- x <- y `binop` z
  | SOracle [Ident] [Ident] -- x... <- Oracle(y...)
  | SFunCall [Ident] Ident [Ident] -- x... <- f(y...)
  | SIfTE Ident Stmt Stmt -- if x then E_1 else E_0
  | SSeq Stmt Stmt -- S_1; S_2
  | SSearch Ident Ident Ident [Ident] -- x, ok <- search(f, [x_1, ... x_{k-1}])
  | SContains Ident Ident [Ident] -- ok <- contains(f, [x_1, ... x_{k-1}])
  deriving (Eq, Show, Read)

data FunDef = FunDef [(Ident, VarType)] [(Ident, VarType)] Stmt -- args@(x_i : T_i) ret_vals@(x'_j : T'_j) body
  deriving (Eq, Show, Read)

type FunCtx = M.Map Ident FunDef
