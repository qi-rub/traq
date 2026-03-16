{-# LANGUAGE TypeApplications #-}

module Traq.Examples.TreeGenerator where

import Traq.Data.Subtyping

import Traq.CPL.Syntax
import Traq.Prelude
import Traq.Primitives
import Traq.Primitives.Amplify.Prelude
import Traq.Primitives.Amplify.QAmplify

treeGeneratorExample ::
  forall ext size prec prim.
  ( Num size
  , Floating prec
  , SizeType ext ~ size
  , PrecType ext ~ prec
  , ext ~ Primitive prim
  , QAmplify size prec :<: prim
  ) =>
  size ->
  size ->
  size ->
  size ->
  Program ext
treeGeneratorExample n w p k =
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
              { param_types = [Arr n (Fin 2), Fin w, Fin n]
              , ret_types = [Arr n (Fin 2), Fin w]
              , mbody =
                  Just
                    ( FunBody
                        { param_names = ["xs", "cur_wt", "i"]
                        , ret_names = ["xs'", "new_wt"]
                        , body_stmt =
                            SeqS
                              [ ExprS{rets = ["xi"], expr = BasicExprE{basic_expr = DynIndexE{arr_expr = VarE{var = "xs"}, ix_expr = VarE{var = "i"}}}}
                              , ExprS{rets = ["wi"], expr = FunCallE{fname = "Weight", args = ["i"]}}
                              , ExprS{rets = ["zero"], expr = BasicExprE{basic_expr = ConstE{val = FinV 0, ty = Fin w}}}
                              , ExprS{rets = ["wi_pick"], expr = BasicExprE{basic_expr = TernaryE{branch = VarE{var = "xi"}, lhs = VarE{var = "wi"}, rhs = VarE{var = "zero"}}}}
                              , ExprS{rets = ["new_wt"], expr = BasicExprE{basic_expr = BinOpE{bin_op = AddOp, lhs = VarE{var = "cur_wt"}, rhs = VarE{var = "wi_pick"}}}}
                              , ExprS{rets = ["xs'"], expr = BasicExprE{basic_expr = VarE{var = "xs"}}}
                              ]
                        }
                    )
              }
        }
    , NamedFunDef
        { fun_name = "TotalWeight"
        , fun_def =
            FunDef
              { param_types = [Arr n (Fin 2)]
              , ret_types = [Fin w]
              , mbody =
                  Just
                    ( FunBody
                        { param_names = ["xs"]
                        , ret_names = ["wt"]
                        , body_stmt =
                            SeqS
                              [ ExprS{rets = ["zero"], expr = BasicExprE{basic_expr = ConstE{val = FinV 0, ty = Fin w}}}
                              , ExprS{rets = ["xs'", "wt"], expr = LoopE{initial_args = ["xs", "zero"], loop_body_fun = "AddWeight"}}
                              ]
                        }
                    )
              }
        }
    , NamedFunDef
        { fun_name = "AddProfit"
        , fun_def =
            FunDef
              { param_types = [Arr n (Fin 2), Fin p, Fin n]
              , ret_types = [Arr n (Fin 2), Fin p]
              , mbody =
                  Just
                    ( FunBody
                        { param_names = ["xs", "cur_pr", "i"]
                        , ret_names = ["xs'", "new_pr"]
                        , body_stmt =
                            SeqS
                              [ ExprS{rets = ["xi"], expr = BasicExprE{basic_expr = DynIndexE{arr_expr = VarE{var = "xs"}, ix_expr = VarE{var = "i"}}}}
                              , ExprS{rets = ["pi"], expr = FunCallE{fname = "Profit", args = ["i"]}}
                              , ExprS{rets = ["zero"], expr = BasicExprE{basic_expr = ConstE{val = FinV 0, ty = Fin p}}}
                              , ExprS{rets = ["pi_pick"], expr = BasicExprE{basic_expr = TernaryE{branch = VarE{var = "xi"}, lhs = VarE{var = "pi"}, rhs = VarE{var = "zero"}}}}
                              , ExprS{rets = ["new_pr"], expr = BasicExprE{basic_expr = BinOpE{bin_op = AddOp, lhs = VarE{var = "cur_pr"}, rhs = VarE{var = "pi_pick"}}}}
                              , ExprS{rets = ["xs'"], expr = BasicExprE{basic_expr = VarE{var = "xs"}}}
                              ]
                        }
                    )
              }
        }
    , NamedFunDef
        { fun_name = "TotalProfit"
        , fun_def =
            FunDef
              { param_types = [Arr n (Fin 2)]
              , ret_types = [Fin p]
              , mbody =
                  Just
                    ( FunBody
                        { param_names = ["xs"]
                        , ret_names = ["pr"]
                        , body_stmt =
                            SeqS
                              [ ExprS{rets = ["zero"], expr = BasicExprE{basic_expr = ConstE{val = FinV 0, ty = Fin p}}}
                              , ExprS{rets = ["xs'", "pr"], expr = LoopE{initial_args = ["xs", "zero"], loop_body_fun = "AddProfit"}}
                              ]
                        }
                    )
              }
        }
    , NamedFunDef
        { fun_name = "TreeGenLoopBody"
        , fun_def =
            FunDef
              { param_types = [Arr n (Fin 2), Fin w, Fin p, Fin n]
              , ret_types = [Arr n (Fin 2), Fin w, Fin p]
              , mbody =
                  Just
                    ( FunBody
                        { param_names = ["xs", "wt", "pr", "i"]
                        , ret_names = ["xs'", "new_wt", "new_pr"]
                        , body_stmt =
                            SeqS
                              [ ExprS{rets = ["xi"], expr = BasicExprE{basic_expr = DynIndexE{arr_expr = VarE{var = "xs"}, ix_expr = VarE{var = "i"}}}}
                              , ExprS{rets = ["y"], expr = RandomSampleE{distr_expr = BernoulliE{prob_one = 0.2}}}
                              , ExprS{rets = ["try_pick"], expr = BasicExprE{basic_expr = BinOpE{bin_op = XorOp, lhs = VarE{var = "xi"}, rhs = VarE{var = "y"}}}}
                              , ExprS{rets = ["wi"], expr = FunCallE{fname = "Weight", args = ["i"]}}
                              , ExprS{rets = ["wt_picked"], expr = BasicExprE{basic_expr = BinOpE{bin_op = AddOp, lhs = VarE{var = "wt"}, rhs = VarE{var = "wi"}}}}
                              , ExprS{rets = ["c"], expr = FunCallE{fname = "Capacity", args = []}}
                              , ExprS{rets = ["can_fit"], expr = BasicExprE{basic_expr = BinOpE{bin_op = LEqOp, lhs = VarE{var = "wt_picked"}, rhs = VarE{var = "c"}}}}
                              , ExprS{rets = ["should_pick"], expr = BasicExprE{basic_expr = BinOpE{bin_op = AndOp, lhs = VarE{var = "try_pick"}, rhs = VarE{var = "can_fit"}}}}
                              , ExprS{rets = ["xs'"], expr = BasicExprE{basic_expr = UpdateArrE{arr_expr = VarE{var = "xs"}, ix_expr = VarE{var = "i"}, rhs = VarE{var = "should_pick"}}}}
                              , ExprS{rets = ["new_wt"], expr = BasicExprE{basic_expr = TernaryE{branch = VarE{var = "should_pick"}, lhs = VarE{var = "wt_picked"}, rhs = VarE{var = "wt"}}}}
                              , ExprS{rets = ["pi"], expr = FunCallE{fname = "Profit", args = ["i"]}}
                              , ExprS{rets = ["pr_picked"], expr = BasicExprE{basic_expr = BinOpE{bin_op = AddOp, lhs = VarE{var = "pr"}, rhs = VarE{var = "pi"}}}}
                              , ExprS{rets = ["new_pr"], expr = BasicExprE{basic_expr = TernaryE{branch = VarE{var = "should_pick"}, lhs = VarE{var = "pr_picked"}, rhs = VarE{var = "pr"}}}}
                              ]
                        }
                    )
              }
        }
    , NamedFunDef
        { fun_name = "TreeGen"
        , fun_def =
            FunDef
              { param_types = [Arr n (Fin 2), Fin p]
              , ret_types = [Fin 2, Arr n (Fin 2)]
              , mbody =
                  Just
                    ( FunBody
                        { param_names = ["xs", "pr"]
                        , ret_names = ["ok", "xs'"]
                        , body_stmt =
                            SeqS
                              [ ExprS{rets = ["zero_wt"], expr = BasicExprE{basic_expr = ConstE{val = FinV 0, ty = Fin w}}}
                              , ExprS{rets = ["zero_pr"], expr = BasicExprE{basic_expr = ConstE{val = FinV 0, ty = Fin p}}}
                              , ExprS{rets = ["xs'", "wt'", "pr'"], expr = LoopE{initial_args = ["xs", "zero_wt", "zero_pr"], loop_body_fun = "TreeGenLoopBody"}}
                              , ExprS{rets = ["ok"], expr = BasicExprE{basic_expr = BinOpE{bin_op = LtOp, lhs = VarE{var = "pr"}, rhs = VarE{var = "pr'"}}}}
                              ]
                        }
                    )
              }
        }
    , NamedFunDef
        { fun_name = "KnapsackLoopBody"
        , fun_def =
            FunDef
              { param_types = [Arr n (Fin 2), Fin k]
              , ret_types = [Arr n (Fin 2)]
              , mbody =
                  Just
                    ( FunBody
                        { param_names = ["xs", "i"]
                        , ret_names = ["xs_next"]
                        , body_stmt =
                            SeqS
                              [ ExprS{rets = ["pr"], expr = FunCallE{fname = "TotalProfit", args = ["xs"]}}
                              , ExprS
                                  { rets = ["ok", "xs'"]
                                  , expr =
                                      PrimCallE
                                        { prim =
                                            Primitive
                                              [PartialFun{pfun_name = "TreeGen", pfun_args = [Just "xs", Just "pr"]}]
                                              (inject (QAmplify @size (Amplify{p_min = 1.0e-2 :: prec})))
                                        }
                                  }
                              , ExprS{rets = ["xs_next"], expr = BasicExprE{basic_expr = TernaryE{branch = VarE{var = "ok"}, lhs = VarE{var = "xs'"}, rhs = VarE{var = "xs"}}}}
                              ]
                        }
                    )
              }
        }
    , NamedFunDef
        { fun_name = "Knapsack"
        , fun_def =
            FunDef
              { param_types = []
              , ret_types = [Arr n (Fin 2)]
              , mbody =
                  Just
                    ( FunBody
                        { param_names = []
                        , ret_names = ["xs'"]
                        , body_stmt =
                            SeqS
                              [ ExprS{rets = ["xs"], expr = BasicExprE{basic_expr = DefaultE{ty = Arr n (Fin 2)}}}
                              , ExprS{rets = ["xs'"], expr = LoopE{initial_args = ["xs"], loop_body_fun = "KnapsackLoopBody"}}
                              ]
                        }
                    )
              }
        }
    ]
