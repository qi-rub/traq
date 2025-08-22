module Traq.Examples.Search where

import qualified Traq.Data.Context as Ctx

import Traq.Prelude
import Traq.Primitives
import Traq.Primitives.Search.Prelude (HasPrimSearch (mkPrimSearch), mkPrimAny)
import Traq.ProtoLang.Syntax

arraySearch :: SizeT -> Program DefaultPrims SizeT
arraySearch n =
  Program
    { funCtx = Ctx.fromList [("Oracle", oracle_decl)]
    , stmt
    }
 where
  oracle_decl :: FunDef DefaultPrims SizeT
  oracle_decl = FunDef{param_types = [Fin n], ret_types = [Fin 2], mbody = Nothing}

  stmt =
    ExprS
      { expr = PrimCallE $ mkPrimAny "Oracle" []
      , rets = ["result"]
      }

arraySearchIx :: SizeT -> Program DefaultPrims SizeT
arraySearchIx n =
  Program
    { funCtx = Ctx.fromList [("Oracle", oracle_decl), ("check", check)]
    , stmt
    }
 where
  oracle_decl :: FunDef DefaultPrims SizeT
  oracle_decl = FunDef{param_types = [Fin n], ret_types = [Fin 2], mbody = Nothing}

  check :: FunDef DefaultPrims SizeT
  check =
    FunDef
      { param_types = [Fin n]
      , mbody =
          Just
            FunBody
              { param_names = ["i"]
              , body_stmt =
                  ExprS
                    { rets = ["b"]
                    , expr = FunCallE{fname = "Oracle", args = ["i"]}
                    }
              , ret_names = ["b"]
              }
      , ret_types = [Fin 2]
      }

  stmt =
    ExprS
      { rets = ["result", "solution"]
      , expr = PrimCallE $ mkPrimSearch "check" []
      }
