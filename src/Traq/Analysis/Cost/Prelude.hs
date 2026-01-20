{-# LANGUAGE ConstraintKinds #-}

module Traq.Analysis.Cost.Prelude where

import Control.Monad.Reader (Reader)

import Traq.Analysis.CostModel.Class
import Traq.Analysis.Prelude
import Traq.Prelude
import Traq.ProtoLang.Eval (EvaluationEnv)

type CostReqs size prec =
  ( Floating prec
  , Num size
  , Ord prec
  , SizeToPrec size prec
  )

type CostModelReqs size prec cost =
  ( CostModel cost
  , prec ~ PrecType cost
  , SizeToPrec size prec
  , Ord cost
  )

type CostAnalysisMonad ext = Reader (EvaluationEnv ext)
