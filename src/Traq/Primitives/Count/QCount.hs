{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |
Quantum Counting

Reference: https://arxiv.org/pdf/quant-ph/9805082
-}
module Traq.Primitives.Count.QCount () where

import Control.Monad (forM, when)
import Control.Monad.Except (throwError)

import qualified Traq.CPL as CPL
import Traq.Prelude
import Traq.Primitives.Class

data QCount size prec = QCount {arg_ty :: CPL.VarType size}
  deriving (Eq, Show, Read)

type instance SizeType (QCount size prec) = size
type instance PrecType (QCount size prec) = prec

newtype QCountFunArg a = QCountFunArg {fun :: a}

type instance PrimFnShape (QCount size prec) = QCountFunArg

instance ValidPrimShape QCountFunArg where
  listToShape [fun] = Right QCountFunArg{fun}
  listToShape _ = Left "count expects exactly one function"

  shapeToList QCountFunArg{fun} = [fun]

instance (Show size) => SerializePrim (QCount size prec) where
  primNames = ["count"]
  parsePrimParams tp _ = QCount <$> CPL.varType tp
  printPrimParams QCount{arg_ty} = [show arg_ty]

-- Type check
instance (Eq size, Integral size) => TypeCheckPrim (QCount size prec) size where
  inferRetTypesPrim QCount{arg_ty} QCountFunArg{fun} = do
    let CPL.FnType param_tys ret_tys = fun

    when (param_tys /= [arg_ty]) $ throwError "count: invalid argument type"
    when (ret_tys /= [CPL.tbool]) $ throwError "count: predicate must return bool."

    let n_items = CPL.domainSize arg_ty
    return [CPL.Fin (n_items + 1)]

instance EvalPrim (QCount size prec) size prec where
  evalPrim QCount{arg_ty} QCountFunArg{fun = fun_eval} = do
    oks <- forM (CPL.domain arg_ty) $ \v -> do
      res <- fun_eval [v]
      case res of
        [b] -> return $ CPL.valueToBool b
        _ -> error "fail"

    let n_sol = length $ filter id oks

    return [CPL.FinV n_sol]

-- ================================================================================
-- Costs (TODO)
-- ================================================================================
