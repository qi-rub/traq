{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Traq.Primitives (
  module Traq.Primitives.Class,
  DefaultPrims (..),
  DefaultPrims',
  QSearchCFNW (..),
) where

import Control.Applicative ((<|>))
import GHC.Generics
import Text.Parsec (try)
import Text.Parsec.Token (GenTokenParser (..))
import Text.Printf (printf)

import qualified Traq.Data.Probability as Prob
import qualified Traq.Data.Symbolic as Sym

import qualified Traq.Analysis as A
import qualified Traq.Analysis.Error.Unitary as A
import qualified Traq.Compiler.Quantum as CompileQ
import qualified Traq.Compiler.Unitary as CompileU
import Traq.Prelude
import Traq.Primitives.Class
import Traq.Primitives.Search.DetSearch
import Traq.Primitives.Search.Prelude
import Traq.Primitives.Search.QSearchCFNW
import Traq.Primitives.Search.RandomSearch
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

data DefaultPrims sizeT precT
  = QAny (Primitive (QSearchCFNW sizeT precT))
  | RAny (Primitive (RandomSearch sizeT precT))
  | DAny (Primitive (DetSearch sizeT precT))
  deriving (Eq, Show, Generic)

type DefaultPrims' = DefaultPrims SizeT Double

type instance SizeType (DefaultPrims sizeT precT) = sizeT
type instance PrecType (DefaultPrims sizeT precT) = precT

instance P.MapSize (DefaultPrims size prec) where
  type MappedSize (DefaultPrims size prec) size' = DefaultPrims size' prec

  mapSize f (QAny p) = QAny (P.mapSize f p)
  mapSize f (RAny p) = RAny (P.mapSize f p)
  mapSize f (DAny p) = DAny (P.mapSize f p)

instance P.RenameVars (DefaultPrims size prec) where
  renameVars prefix (QAny p) = QAny $ P.renameVars prefix p
  renameVars prefix (RAny p) = RAny $ P.renameVars prefix p
  renameVars prefix (DAny p) = DAny $ P.renameVars prefix p

printSearchLikePrim :: (Show size) => String -> P.VarType size -> [PartialFun] -> String
printSearchLikePrim name ty [pfun] = printf "@%s<%s>[%s]" name (str_trim ty) (str_trim pfun)
 where
  str_trim :: forall a. (PP.ToCodeString a) => a -> String
  str_trim = trim . PP.toCodeString

  trim = head . lines
printSearchLikePrim _ _ _ = error "primitive: expected exactly one predicate"

-- Printing
instance (Show sizeT) => PP.ToCodeString (DefaultPrims sizeT precT) where
  build (QAny (Primitive predfun (QSearchCFNW (PrimSearch AnyK ty)))) = PP.putWord $ printSearchLikePrim "any" ty predfun
  build (QAny (Primitive predfun (QSearchCFNW (PrimSearch SearchK ty)))) = PP.putWord $ printSearchLikePrim "search" ty predfun
  build (RAny (Primitive predfun (RandomSearch (PrimSearch AnyK ty)))) = PP.putWord $ printSearchLikePrim "any_rand" ty predfun
  build (DAny (Primitive predfun (DetSearch (PrimSearch AnyK ty)))) = PP.putWord $ printSearchLikePrim "any_det" ty predfun
  build _ = fail "DefaultPrims: invalid primitive"

-- Parsing
instance (sizeT ~ Sym.Sym SizeT) => P.Parseable (DefaultPrims sizeT precT) where
  parseE tp = do
    s <- foldr1 (<|>) $ map (try . symbol tp) ["@any", "@search", "@any_rand", "@any_det"]
    ty <- angles tp $ P.varType tp
    pred_fun <- brackets tp (P.parseE tp)

    case s of
      "@any" -> pure $ QAny . Primitive [pred_fun] $ QSearchCFNW (PrimSearch AnyK ty)
      "@search" -> pure $ QAny . Primitive [pred_fun] $ QSearchCFNW (PrimSearch SearchK ty)
      "@any_rand" -> pure $ RAny . Primitive [pred_fun] $ RandomSearch (PrimSearch AnyK ty)
      "@any_det" -> pure $ DAny . Primitive [pred_fun] $ DetSearch (PrimSearch AnyK ty)
      _ -> fail ""

instance (P.TypingReqs sizeT) => P.TypeInferrable (DefaultPrims sizeT precT) sizeT
instance P.HasFreeVars (DefaultPrims sizeT precT)

-- Evaluation
instance (P.EvalReqs sizeT precT) => P.Evaluatable (DefaultPrims sizeT precT) sizeT precT

-- Error Analysis
instance
  (A.ErrorReqs size prec, Show size, Integral size) =>
  A.TraceNormErrorU (A.AnnFailProb (DefaultPrims size prec)) size prec
  where
  traceNormErrorU (A.AnnFailProb eps (QAny p)) = A.traceNormErrorU (A.AnnFailProb eps p)
  traceNormErrorU (A.AnnFailProb eps (RAny p)) = A.traceNormErrorU (A.AnnFailProb eps p)
  traceNormErrorU (A.AnnFailProb eps (DAny p)) = A.traceNormErrorU (A.AnnFailProb eps p)

instance
  (A.ErrorReqs size prec, Show size, Integral size, A.SizeToPrec size prec) =>
  A.TVErrorQ (A.AnnFailProb (DefaultPrims size prec)) size prec
  where
  tvErrorQ (A.AnnFailProb eps (QAny p)) = A.tvErrorQ (A.AnnFailProb eps p)
  tvErrorQ (A.AnnFailProb eps (RAny p)) = A.tvErrorQ (A.AnnFailProb eps p)
  tvErrorQ (A.AnnFailProb eps (DAny p)) = A.tvErrorQ (A.AnnFailProb eps p)

-- Cost Analysis
instance
  (A.CostReqs size prec, Show size, Integral size, A.SizeToPrec size prec) =>
  A.CostU (A.AnnFailProb (DefaultPrims size prec)) size prec
  where
  costU (A.AnnFailProb eps (QAny p)) = A.costU (A.AnnFailProb eps p)
  costU (A.AnnFailProb eps (RAny p)) = A.costU (A.AnnFailProb eps p)
  costU (A.AnnFailProb eps (DAny p)) = A.costU (A.AnnFailProb eps p)

instance
  (A.CostReqs size prec, Show size, Integral size, A.SizeToPrec size prec) =>
  A.CostQ (A.AnnFailProb (DefaultPrims size prec)) size prec
  where
  costQ (A.AnnFailProb eps (QAny p)) = A.costQ (A.AnnFailProb eps p)
  costQ (A.AnnFailProb eps (RAny p)) = A.costQ (A.AnnFailProb eps p)
  costQ (A.AnnFailProb eps (DAny p)) = A.costQ (A.AnnFailProb eps p)

instance
  (A.CostReqs size prec, P.EvalReqs size prec, A.SizeToPrec size prec) =>
  A.ExpCostQ (A.AnnFailProb (DefaultPrims size prec)) size prec
  where
  expCostQ (A.AnnFailProb eps (QAny p)) = A.expCostQ (A.AnnFailProb eps p)
  expCostQ (A.AnnFailProb eps (RAny p)) = A.expCostQ (A.AnnFailProb eps p)
  expCostQ (A.AnnFailProb eps (DAny p)) = A.expCostQ (A.AnnFailProb eps p)

-- Costs
instance
  ( Integral sizeT
  , Floating precT
  , Show precT
  , P.TypingReqs sizeT
  ) =>
  A.UnitaryCost (DefaultPrims sizeT precT) sizeT precT

instance
  ( Integral sizeT
  , Floating precT
  , Ord precT
  , Show precT
  , P.TypingReqs sizeT
  ) =>
  A.QuantumHavocCost (DefaultPrims sizeT precT) sizeT precT

instance
  ( Floating precT
  , Ord precT
  , Prob.ProbType precT
  , sizeT ~ SizeT
  , Show precT
  , P.EvalReqs sizeT precT
  ) =>
  A.QuantumExpCost (DefaultPrims sizeT precT) sizeT precT

-- Lowering
instance
  ( Integral sizeT
  , Floating precT
  , RealFloat precT
  , P.TypingReqs sizeT
  , Show precT
  , A.SizeToPrec sizeT precT
  ) =>
  CompileU.Lowerable (DefaultPrims sizeT precT) sizeT precT
  where
  lowerPrimitive delta (QAny q) = CompileU.lowerPrimitive delta q
  lowerPrimitive _ _ = error "TODO: lowerPrimitive"

instance
  ( Integral sizeT
  , Floating precT
  , RealFloat precT
  , P.TypingReqs sizeT
  , Show precT
  , sizeT ~ SizeT
  ) =>
  CompileQ.Lowerable (DefaultPrims sizeT precT) sizeT precT
  where
  lowerPrimitive eps (QAny q) = CompileQ.lowerPrimitive eps q
  lowerPrimitive _ _ = error "TODO: lowerPrimitive"
