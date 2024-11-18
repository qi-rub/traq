module QCompose.ProtoLang where

import qualified Data.Map as M

-- proto-search language
data VarType = Fin { bitsize :: Int }
  deriving (Eq, Show, Read)

pbool :: VarType
pbool = Fin 1

range :: VarType -> [Int]
range (Fin n) = [0..2^n - 1]

data PFunType = PFunType [VarType] [VarType]
  deriving (Eq, Show, Read)

type Ident = String

data UnOp = PNot
  deriving (Eq, Show, Read)
data BinOp = PAdd | PLeq | PAnd
  deriving (Eq, Show, Read)

data Expr =
  EConst Int VarType -- const v :: T
  | EVar Ident -- x
  | EUnOp UnOp Ident -- op x
  | EBinOp BinOp Ident Ident -- x `binop` y
  | EOracle [Ident]
  | ECall Ident [Ident]
  | EIfTE Ident Expr Expr -- if x then E_1 else E_0
  | ESearch Ident [Ident] -- f, [x_1, ... x_{k-1}]
  deriving (Eq, Show, Read)

data Stmt =
  SReturn Ident 
  | SLet [Ident] Expr Stmt
  deriving (Eq, Show, Read)

data FunDef = FunDef PFunType [Ident] Stmt -- fun-type, param-names, body
  deriving (Eq, Show, Read)

type FunCtx = M.Map Ident FunDef

-- cq-while language
data CQWhile =
  -- simple
  Skip
  -- sequences
  | Seq [CQWhile]
  | Repeat Int CQWhile
  -- classical
  | CNew Ident VarType
  | CAssign Ident -- Expr?
  | CRandom Ident
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

class CompileClassical a where
  compile_classical :: a -> CQWhile

instance CompileClassical Expr where
  compile_classical = 