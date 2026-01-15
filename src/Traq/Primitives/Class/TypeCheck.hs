{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.Primitives.Class.TypeCheck (
  TypeCheckPrim (..),
) where

import Control.Monad (forM, when)
import Control.Monad.Except (throwError)
import Data.Maybe (catMaybes)
import Text.Printf (printf)

import Lens.Micro.Mtl

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx

import Traq.Prelude
import Traq.Primitives.Class.Prelude
import qualified Traq.ProtoLang as P

-- --------------------------------------------------------------------------------
-- Typing
-- --------------------------------------------------------------------------------

{- | Type check a primitive given the types of its function arguments, and infer the return types.
The typechecker internally checks that the bound arguments are correct,
and only gives the user the final type of the partial function.
-}
class
  ( size ~ SizeType prim
  , ValidPrimShape (PrimFnShape prim)
  ) =>
  TypeCheckPrim prim size
    | prim -> size
  where
  inferRetTypesPrim ::
    forall ext' shape m.
    ( m ~ P.TypeChecker ext'
    , size ~ SizeType ext'
    , shape ~ PrimFnShape prim
    ) =>
    prim ->
    shape (P.FnType size) ->
    m [P.VarType size]

instance (TypeCheckPrim prim size, P.TypingReqs size) => P.TypeInferrable (Primitive prim) size where
  inferTypes (Primitive par_funs prim) = do
    fn_tys <- forM par_funs $ \PartialFun{pfun_name, pfun_args} -> do
      P.FunDef{P.param_types, P.ret_types} <-
        view (Ctx.at pfun_name)
          >>= maybeWithError (printf "cannot find function argument `%s`" pfun_name)

      when (length pfun_args /= length param_types) $
        throwError "Invalid number of function arguments"

      prim_arg_tys <- forM (zip pfun_args param_types) $ \(mvar, ty) -> do
        case mvar of
          Just var -> do
            var_ty <- Ctx.lookup var
            when (var_ty /= ty) $ throwError "invalid arg type to bind"
            return Nothing
          Nothing -> return $ Just ty

      return $ P.FnType (catMaybes prim_arg_tys) ret_types

    shaped_fn_tys <- liftEither $ listToShape fn_tys

    inferRetTypesPrim prim shaped_fn_tys
