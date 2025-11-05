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

import Traq.Prelude
import Traq.Primitives.Class
import qualified Traq.ProtoLang as P

data QCount sizeT precT = QCount {arg_ty :: P.VarType sizeT}
  deriving (Eq, Show, Read)

type instance SizeType (QCount sizeT precT) = sizeT
type instance PrecType (QCount sizeT precT) = precT

newtype QCountFunArg a = QCountFunArg {fun :: a}

type instance PrimFnShape (QCount size prec) = QCountFunArg

instance ValidPrimShape QCountFunArg where
  listToShape [fun] = Right QCountFunArg{fun}
  listToShape _ = Left "count expects exactly one function"

  shapeToList QCountFunArg{fun} = [fun]

instance (Show sizeT) => SerializePrim (QCount sizeT precT) where
  primNames = ["count"]
  parsePrimParams tp _ = QCount <$> P.varType tp
  printPrimParams QCount{arg_ty} = [show arg_ty]

-- Type check
instance (Eq sizeT, Integral sizeT) => TypeCheckPrim (QCount sizeT precT) sizeT where
  inferRetTypesPrim QCount{arg_ty} QCountFunArg{fun} = do
    let P.FnType param_tys ret_tys = fun

    when (param_tys /= [arg_ty]) $ throwError "count: invalid argument type"
    when (ret_tys /= [P.tbool]) $ throwError "count: predicate must return bool."

    let n_items = P.domainSize arg_ty
    return [P.Fin (n_items + 1)]

instance EvalPrim (QCount size prec) size prec where
  evalPrim QCount{arg_ty} QCountFunArg{fun = fun_eval} = do
    oks <- forM (P.domain arg_ty) $ \v -> do
      res <- fun_eval [v]
      case res of
        [b] -> return $ P.valueToBool b
        _ -> error "fail"

    let n_sol = length $ filter id oks

    return [P.FinV n_sol]

-- ================================================================================
-- Costs (TODO)
-- ================================================================================
