{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module QCompose.Primitives.TreeSearch (
  TreeSearch (..),
) where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Data.Maybe (fromMaybe)
import Lens.Micro
import Lens.Micro.Mtl
import Text.Parsec.Token (GenTokenParser (..))
import Text.Printf (printf)

import QCompose.Control.Monad (maybeWithError)
import qualified QCompose.Data.Context as Ctx

import QCompose.Prelude
import qualified QCompose.ProtoLang as P
import QCompose.Utils.Printing

{- | Search a binary tree rooted at node @1@.

 @getChildren@ returns the two child nodes.
 If any is @0@ it means there no node.
 Both being @0@ means the current node is a leaf.

 @checkNode@ returns a boolean value.
-}
data TreeSearch = TreeSearch {getChildren :: Ident, checkNode :: Ident}
  deriving (Eq, Show, Read)

-- Printing
instance ToCodeString TreeSearch where
  toCodeString TreeSearch{getChildren, checkNode} = printf "@treesearch[%s, %s]" getChildren checkNode

-- Parsing
instance P.CanParsePrimitive TreeSearch where
  primitiveParser tp = do
    symbol tp "@treesearch"
    (getChildren, checkNode) <- brackets tp $ do
      gc <- identifier tp
      comma tp
      cn <- identifier tp
      return (gc, cn)
    return TreeSearch{getChildren, checkNode}

-- Type check
instance P.TypeCheckablePrimitive TreeSearch sizeT where
  typeCheckPrimitive TreeSearch{getChildren, checkNode} args = do
    P.FunDef{P.param_types = gc_param_tys, P.ret_types = gc_ret_tys} <-
      view (Ctx.at getChildren)
        >>= maybeWithError (printf "cannot find getChildren function `%s`" getChildren)

    P.FunDef{P.param_types = cn_param_tys, P.ret_types = cn_ret_tys} <-
      view (Ctx.at checkNode)
        >>= maybeWithError (printf "cannot find checkNode function `%s`" checkNode)

    when (last gc_param_tys /= last cn_param_tys) $
      throwError "node type does not match"

    let node_ty = last gc_param_tys

    when (gc_ret_tys /= [node_ty, node_ty]) $
      throwError "getChildren must return exactly two nodes"

    when (cn_ret_tys /= [P.tbool]) $
      throwError "checkNode must return a Bool"

    arg_tys <- mapM Ctx.lookup args
    when (init gc_param_tys ++ init cn_param_tys /= arg_tys) $
      throwError "Invalid arguments to bind to getChildren & checkNode"

    return [P.tbool]

-- | Evaluation
runTreeSearch :: (Monad m) => (Value -> m (Value, Value)) -> (Value -> m Bool) -> Value -> m Bool
runTreeSearch _ _ 0 = return False
runTreeSearch child check u = do
  ok <- check u
  if ok
    then return True
    else do
      (l, r) <- child u
      (||) <$> check l <*> check r

instance
  (P.EvaluatablePrimitive primsT primsT) =>
  P.EvaluatablePrimitive primsT TreeSearch
  where
  evalPrimitive TreeSearch{getChildren, checkNode} args = do
    child_fun <- view $ _1 . Ctx.at getChildren . to (fromMaybe (error "unable to find predicate, please typecheck first!"))
    check_fun <- view $ _1 . Ctx.at checkNode . to (fromMaybe (error "unable to find predicate, please typecheck first!"))

    let (child_args, check_args) = splitAt (length $ P.param_types child_fun) args

    let nxt u =
          ( do
              cs <- P.evalFun (child_args ++ [u]) getChildren child_fun
              return (head cs, cs !! 1)
          )
    let chk u =
          ( do
              vs <- P.evalFun (check_args ++ [u]) checkNode check_fun
              let ok = head vs
              return $ ok /= 0
          )

    let root = 1
    ok <- runTreeSearch nxt chk root

    return [P.boolToValue ok]
