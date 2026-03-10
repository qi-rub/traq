module Traq.QPL.Memory (numQubits) where

import Traq.CPL (VarType (..))
import Traq.Prelude
import Traq.QPL.Syntax

log2 :: (Integral a) => a -> a
log2 n = ceiling $ logBase (2 :: Double) (fromIntegral n)

numQubitsForType :: VarType SizeT -> SizeT
numQubitsForType (Fin n) = log2 n
numQubitsForType (Arr n t) = n * numQubitsForType t
numQubitsForType (Tup ts) = sum $ map numQubitsForType ts
numQubitsForType (Bitvec n) = n

numQubitsForProc :: ProcDef SizeT -> SizeT
numQubitsForProc p@ProcDef{proc_param_types}
  | isUProc p = sum $ map numQubitsForType proc_param_types
numQubitsForProc _ = 0

numQubits :: Program SizeT -> SizeT
numQubits (Program ps) = maximum . map numQubitsForProc $ ps
