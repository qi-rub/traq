{-# LANGUAGE TypeApplications #-}

module Traq.Examples.Search where

import Traq.Prelude
import Traq.Primitives
import Traq.Primitives.Search.Prelude (PrimAny (..), PrimSearch (..))
import Traq.ProtoLang.Syntax

arraySearch :: forall precT. SizeT -> Program (DefaultPrims SizeT precT)
arraySearch n = Program [NamedFunDef "Oracle" oracle_decl, NamedFunDef "main" main_def]
 where
  oracle_decl :: FunDef (DefaultPrims SizeT precT)
  oracle_decl = FunDef{param_types = [Fin n], ret_types = [Fin 2], mbody = Nothing}

  main_def :: FunDef (DefaultPrims SizeT precT)
  main_def =
    FunDef
      { param_types = []
      , ret_types = [Fin 2]
      , mbody =
          Just
            FunBody
              { param_names = []
              , ret_names = ["result"]
              , body_stmt =
                  ExprS
                    { expr = primCallE @(PrimAny SizeT precT) $ PrimAny "Oracle" []
                    , rets = ["result"]
                    }
              }
      }

arraySearchIx :: forall precT. SizeT -> Program (DefaultPrims SizeT precT)
arraySearchIx n = Program [NamedFunDef "Oracle" oracle_decl, NamedFunDef "check" check, NamedFunDef "main" main_def]
 where
  oracle_decl :: FunDef (DefaultPrims SizeT precT)
  oracle_decl = FunDef{param_types = [Fin n], ret_types = [Fin 2], mbody = Nothing}

  check :: FunDef (DefaultPrims SizeT precT)
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

  main_def :: FunDef (DefaultPrims SizeT precT)
  main_def =
    FunDef
      { param_types = []
      , mbody =
          Just
            FunBody
              { param_names = []
              , body_stmt =
                  ExprS
                    { rets = ["result", "solution"]
                    , expr = primCallE @(PrimSearch SizeT precT) $ PrimSearch "check" []
                    }
              , ret_names = ["result", "solution"]
              }
      , ret_types = [Fin 2, Fin n]
      }
