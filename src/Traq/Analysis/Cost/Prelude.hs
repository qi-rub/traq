{-# LANGUAGE ConstraintKinds #-}

module Traq.Analysis.Cost.Prelude where

import Control.Monad.Reader (Reader)

import Traq.ProtoLang.Eval (EvaluationEnv)

type CostReqs size prec =
  ( Floating prec
  , Num size
  , Ord prec
  )

type CostAnalysisMonad ext = Reader (EvaluationEnv ext)
