module Traq.CQPL.Memory (numQubits) where

import qualified Traq.Data.Context as Ctx

import Traq.CQPL.Syntax
import Traq.Prelude
import Traq.ProtoLang (VarType (..))

numQubitsForType :: VarType SizeT -> SizeT
numQubitsForType (Fin n) = ceiling $ logBase (2 :: Double) (fromIntegral n)

numQubitsForProc :: ProcDef h SizeT c -> SizeT
numQubitsForProc p@ProcDef{proc_param_types}
  | isUProc p = sum $ map numQubitsForType proc_param_types
numQubitsForProc _ = 0

numQubits :: Program h SizeT c -> SizeT
numQubits Program{proc_defs} = maximum . map numQubitsForProc $ Ctx.elems proc_defs
