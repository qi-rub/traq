module Traq.CQPL.Memory (numQubits) where

import qualified Traq.Data.Context as Ctx

import Traq.CQPL.Syntax
import Traq.Prelude
import Traq.ProtoLang (VarType (..))

log2 :: (Integral a) => a -> a
log2 n = ceiling $ logBase (2 :: Double) (fromIntegral n)

numQubitsForType :: VarType SizeT -> SizeT
numQubitsForType (Fin n) = log2 n
numQubitsForType (Arr n t) = n * numQubitsForType t
numQubitsForType (Tup ts) = sum $ map numQubitsForType ts

numQubitsForProc :: ProcDef SizeT -> SizeT
numQubitsForProc p@ProcDef{proc_param_types}
  | isUProc p = sum $ map numQubitsForType proc_param_types
numQubitsForProc _ = 0

numQubits :: Program SizeT -> SizeT
numQubits Program{proc_defs} = maximum . map numQubitsForProc $ Ctx.elems proc_defs
