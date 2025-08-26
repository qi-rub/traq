module Traq.Examples.TreeGenerator where

import Traq.ProtoLang.Syntax

treeGeneratorExample :: (Num sizeT) => sizeT -> sizeT -> sizeT -> Program primT sizeT
treeGeneratorExample n w p =
  Program
    [ NamedFunDef
        { fun_name = "Capacity"
        , fun_def =
            FunDef
              { param_types = []
              , ret_types = [Fin w]
              , mbody = Nothing
              }
        }
    , NamedFunDef
        { fun_name = "Profit"
        , fun_def =
            FunDef
              { param_types = [Fin n]
              , ret_types = [Fin p]
              , mbody = Nothing
              }
        }
    , NamedFunDef
        { fun_name = "Weight"
        , fun_def =
            FunDef
              { param_types = [Fin n]
              , ret_types = [Fin w]
              , mbody = Nothing
              }
        }
    , NamedFunDef
        { fun_name = "AddWeight"
        , fun_def =
            FunDef
              { param_types = [Fin w, Arr n (Fin 2), Fin n]
              , ret_types = [Fin w, Arr n (Fin 2)]
              , mbody =
                  Just
                    FunBody
                      { param_names = ["acc", "xs", "i"]
                      , ret_names = ["acc'", "xs'"]
                      , body_stmt =
                          SeqS
                            [ ExprS ["xi"] $
                                BasicExprE $
                                  DynIndexE (VarE "xs") (VarE "i")
                            , ExprS ["wi"] $
                                FunCallE "Weight" ["i"]
                            , ExprS ["zero"] $
                                BasicExprE $
                                  ConstE (FinV 0) (Fin w)
                            , ExprS ["mult"] $
                                BasicExprE $
                                  TernaryE (VarE "xi") (VarE "wi") (VarE "zero")
                            , ExprS ["acc'"] $
                                BasicExprE $
                                  BinOpE AddOp (VarE "mult") (VarE "acc")
                            , ExprS ["xs'"] $
                                BasicExprE $
                                  VarE "xs"
                            ]
                      }
              }
        }
    , NamedFunDef
        { fun_name = "ComputeProfit"
        , fun_def =
            FunDef
              { param_types = [Arr n (Fin 2)]
              , ret_types = [Fin w]
              , mbody =
                  Just
                    FunBody
                      { param_names = ["xs"]
                      , ret_names = ["tw"]
                      , body_stmt =
                          SeqS
                            [ ExprS ["acc"] $
                                BasicExprE $
                                  ConstE (FinV 0) (Fin w)
                            , ExprS ["tw", "_xs'"] $
                                LoopE
                                  { initial_args = [VarE "acc", VarE "xs"]
                                  , loop_body_fun = "AddWeight"
                                  }
                            ]
                      }
              }
        }
    , NamedFunDef
        { fun_name = "SampleCheckUpdateAdd"
        , fun_def =
            FunDef
              { param_types = [Fin w, Arr n (Fin 2), Fin n]
              , ret_types = [Fin w, Arr n (Fin 2)]
              , mbody =
                  Just
                    FunBody
                      { param_names = ["tw", "xs", "i"]
                      , ret_names = ["tw'", "xs'"]
                      , body_stmt =
                          SeqS
                            [ ExprS ["y"] $
                                UniformRandomE (Fin 2)
                            , ExprS ["bit"] $
                                BasicExprE $
                                  DynIndexE (VarE "xs") (VarE "i")
                            , ExprS ["new"] $
                                BasicExprE $
                                  BinOpE XorOp (VarE "bit") (VarE "y")
                            , ExprS ["wi"] $
                                FunCallE "Weight" ["i"]
                            , ExprS ["temp"] $
                                BasicExprE $
                                  BinOpE AddOp (VarE "tw") (VarE "wi")
                            , ExprS ["c"] $
                                FunCallE "Capacity" []
                            , ExprS ["ok"] $
                                BasicExprE $
                                  BinOpE LEqOp (VarE "temp") (VarE "c")
                            , ExprS ["should_pick"] $
                                BasicExprE $
                                  BinOpE AndOp (VarE "new") (VarE "ok")
                            , ExprS ["xs'"] $
                                BasicExprE $
                                  UpdateArrE (VarE "xs") (VarE "i") (VarE "should_pick")
                            , ExprS ["zero"] $
                                BasicExprE $
                                  ConstE (FinV 0) (Fin w)
                            , ExprS ["temp2"] $
                                BasicExprE $
                                  TernaryE (VarE "should_pick") (VarE "wi") (VarE "zero")
                            , ExprS ["tw'"] $
                                BasicExprE $
                                  BinOpE AddOp (VarE "tw") (VarE "temp2")
                            ]
                      }
              }
        }
    , NamedFunDef
        { fun_name = "Sampler"
        , fun_def =
            FunDef
              { param_types = [Arr n (Fin 2)]
              , ret_types = [Fin 2, Arr n (Fin 2)]
              , mbody =
                  Just
                    FunBody
                      { param_names = ["xs"]
                      , ret_names = ["flag", "xs'"]
                      , body_stmt =
                          SeqS
                            [ ExprS ["acc"] $
                                BasicExprE $
                                  ConstE (FinV 0) (Fin w)
                            , ExprS ["total_weight", "xs'"] $
                                LoopE
                                  { initial_args = [VarE "acc", VarE "xs"]
                                  , loop_body_fun = "SampleCheckUpdateAdd"
                                  }
                            , ExprS ["prof_prev"] $
                                FunCallE "ComputeProfit" ["xs"]
                            , ExprS ["prof_new"] $
                                FunCallE "ComputeProfit" ["xs'"]
                            , ExprS ["flag"] $
                                BasicExprE $
                                  BinOpE LtOp (VarE "prof_prev") (VarE "prof_new")
                            ]
                      }
              }
        }
    ]
