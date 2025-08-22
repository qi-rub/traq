{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.Primitives.TreeSearch (
  TreeSearch (..),
) where

import Control.Monad (forM, when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Maybe (fromMaybe)
import Text.Parsec.Token (GenTokenParser (..))
import Text.Printf (printf)

import Lens.Micro.GHC
import Lens.Micro.Mtl

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx

import Traq.Prelude
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

{- | Search a binary tree rooted at node @1@.

 @getChildren@ returns the two child nodes.
 If any is @0@ it means there no node.
 Both being @0@ means the current node is a leaf.

 @checkNode@ returns a boolean value.
-}
data TreeSearch = TreeSearch
  { getChildren :: Ident
  , getChildrenArgs :: [Ident]
  , checkNode :: Ident
  , checkNodeArgs :: [Ident]
  }
  deriving (Eq, Show, Read)

-- Printing
instance PP.ToCodeString TreeSearch where
  build TreeSearch{getChildren, getChildrenArgs, checkNode, checkNodeArgs} =
    PP.putWord $
      printf
        "@treesearch[%s, %s](%s)(%s)"
        getChildren
        checkNode
        (PP.commaList getChildrenArgs)
        (PP.commaList checkNodeArgs)

-- Parsing
instance P.CanParsePrimitive TreeSearch where
  primitiveParser tp = do
    symbol tp "@treesearch"
    (getChildren, checkNode) <- brackets tp $ do
      gc <- identifier tp
      comma tp
      cn <- identifier tp
      return (gc, cn)
    getChildrenArgs <- parens tp $ commaSep tp $ identifier tp
    checkNodeArgs <- parens tp $ commaSep tp $ identifier tp
    return TreeSearch{getChildren, getChildrenArgs, checkNode, checkNodeArgs}

-- Type check
instance P.TypeCheckablePrimitive TreeSearch where
  typeCheckPrimitive TreeSearch{getChildren, getChildrenArgs, checkNode, checkNodeArgs} = do
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

    gc_arg_tys <- mapM Ctx.lookup getChildrenArgs
    when (init gc_param_tys /= gc_arg_tys) $
      throwError "Invalid arguments to bind to getChildren"

    cn_arg_tys <- mapM Ctx.lookup checkNodeArgs
    when (init cn_param_tys /= cn_arg_tys) $
      throwError "Invalid arguments to bind to checkNode"

    return [P.tbool]

-- | Evaluation
runTreeSearch ::
  (Monad m, sizeT ~ SizeT) =>
  (P.Value sizeT -> m (P.Value sizeT, P.Value sizeT)) ->
  (P.Value sizeT -> m Bool) ->
  P.Value sizeT ->
  m Bool
runTreeSearch _ _ (P.FinV 0) = return False
runTreeSearch child check u = do
  ok <- check u
  if ok
    then return True
    else do
      (l, r) <- child u
      (||) <$> check l <*> check r

instance
  ( Fractional costT
  , P.EvaluatablePrimitive primsT primsT costT
  ) =>
  P.EvaluatablePrimitive primsT TreeSearch costT
  where
  evalPrimitive TreeSearch{getChildren, getChildrenArgs, checkNode, checkNodeArgs} sigma = do
    child_fun <- view $ P._funCtx . Ctx.at getChildren . to (fromMaybe (error "unable to find predicate, please typecheck first!"))
    check_fun <- view $ P._funCtx . Ctx.at checkNode . to (fromMaybe (error "unable to find predicate, please typecheck first!"))

    child_args <- runReaderT ?? sigma $ forM getChildrenArgs $ \x -> do
      view $ Ctx.at x . non (error "invalid arg")
    check_args <- runReaderT ?? sigma $ forM checkNodeArgs $ \x -> do
      view $ Ctx.at x . non (error "invalid arg")

    let nxt u =
          ( do
              cs <- P.evalFun (child_args ++ [u]) getChildren child_fun
              return (head cs, cs !! 1)
          )
    let chk u =
          ( do
              vs <- P.evalFun (check_args ++ [u]) checkNode check_fun
              let ok = head vs
              return $ P.valueToBool ok
          )

    let root = P.FinV 1
    ok <- runTreeSearch nxt chk root

    return [P.toValue ok]
