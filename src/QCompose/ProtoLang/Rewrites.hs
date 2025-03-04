module QCompose.ProtoLang.Rewrites where

import Control.Monad (foldM, guard)
import Data.Maybe (isJust)
import qualified Data.Set as S
import QCompose.ProtoLang.Syntax
import QCompose.ProtoLang.Vars

-- | Flatten nested 'SeqS' instructions
flattenSeq :: Stmt a -> Maybe (Stmt a)
flattenSeq (SeqS [s]) = Just s
flattenSeq (SeqS ss)
  | any isSeq ss =
      Just . fromList . concatMap toList $ ss
  where
    isSeq :: Stmt a -> Bool
    isSeq (SeqS _) = True
    isSeq _ = False

    toList :: Stmt a -> [Stmt a]
    toList (SeqS xs) = xs
    toList x = [x]

    fromList :: [Stmt a] -> Stmt a
    fromList [x] = x
    fromList xs = SeqS xs
flattenSeq _ = Nothing

{- | Convert all 'SeqS' into a list of size at most 2.
  For example, @SeqS [a, b, c]@ becomes @SeqS [a, SeqS [b, c]]@
-}
pairSeq :: Stmt a -> Stmt a
pairSeq (SeqS ss) | not (null ss) = foldr1 (\a b -> SeqS [a, b]) ss
pairSeq s = s

-- | Check if a program has unique variable names
checkUniqueVars :: Program a -> Bool
checkUniqueVars Program{funCtx = FunCtx{funDefs}, stmt} =
  isJust . foldM combine S.empty $ allVars stmt : map allFunVars funDefs
  where
    allFunVars :: FunDef a -> VarSet
    allFunVars FunDef{params, body} = S.union (allVars body) (S.fromList $ map fst params)

    combine :: VarSet -> VarSet -> Maybe VarSet
    combine u v = do
      guard $ S.disjoint u v
      return $ S.union u v
