{-# LANGUAGE TypeApplications #-}

module Traq.Examples.TreeGenerator where

import Traq.Data.Subtyping

import Traq.Analysis (SizeToPrec (..))
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
  , SizeToPrec size prec
  ) =>
  size ->
  size ->
  size ->
  size ->
  prec ->
  Program ext
treeGeneratorExample n w p k bernoulli_prob =
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
        { fun_name = "TotalWeight"
        , fun_def =
            FunDef
              { param_types = [Arr n (Fin 2)]
              , ret_types = [Fin w]
              , mbody =
                  Just
                    FunBody
                      { param_names = ["xs"]
                      , ret_names = ["wt"]
                      , body_stmt =
                          SeqS
                            [ ExprS{rets = ["zero"], expr = BasicExprE{basic_expr = ConstE{val = FinV 0, ty = Fin w}}}
                            , ExprS{rets = ["wt"], expr = BasicExprE{basic_expr = ConstE{val = FinV 0, ty = Fin w}}}
                            , ForS
                                { loop_ix = "i"
                                , loop_ty = Fin n
                                , loop_body =
                                    SeqS
                                      [ ExprS{rets = ["xi"], expr = BasicExprE{basic_expr = DynIndexE{arr_expr = VarE{var = "xs"}, ix_expr = VarE{var = "i"}}}}
                                      , ExprS{rets = ["wi"], expr = FunCallE{fname = "Weight", args = ["i"]}}
                                      , ExprS{rets = ["wi"], expr = BasicExprE{basic_expr = TernaryE{branch = VarE{var = "xi"}, lhs = VarE{var = "wi"}, rhs = VarE{var = "zero"}}}}
                                      , ExprS{rets = ["wt"], expr = BasicExprE{basic_expr = BinOpE{bin_op = AddOp, lhs = VarE{var = "wt"}, rhs = VarE{var = "wi"}}}}
                                      ]
                                }
                            ]
                      }
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
                    FunBody
                      { param_names = ["xs"]
                      , ret_names = ["pr"]
                      , body_stmt =
                          SeqS
                            [ ExprS{rets = ["zero"], expr = BasicExprE{basic_expr = ConstE{val = FinV 0, ty = Fin p}}}
                            , ExprS{rets = ["pr"], expr = BasicExprE{basic_expr = ConstE{val = FinV 0, ty = Fin p}}}
                            , ForS
                                { loop_ix = "i"
                                , loop_ty = Fin n
                                , loop_body =
                                    SeqS
                                      [ ExprS{rets = ["xi"], expr = BasicExprE{basic_expr = DynIndexE{arr_expr = VarE{var = "xs"}, ix_expr = VarE{var = "i"}}}}
                                      , ExprS{rets = ["pi"], expr = FunCallE{fname = "Profit", args = ["i"]}}
                                      , ExprS{rets = ["pi"], expr = BasicExprE{basic_expr = TernaryE{branch = VarE{var = "xi"}, lhs = VarE{var = "pi"}, rhs = VarE{var = "zero"}}}}
                                      , ExprS{rets = ["pr"], expr = BasicExprE{basic_expr = BinOpE{bin_op = AddOp, lhs = VarE{var = "pr"}, rhs = VarE{var = "pi"}}}}
                                      ]
                                }
                            ]
                      }
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
                    FunBody
                      { param_names = ["xs", "old_pr"]
                      , ret_names = ["ok", "xs"]
                      , body_stmt =
                          SeqS
                            [ ExprS{rets = ["wt"], expr = BasicExprE{basic_expr = ConstE{val = FinV 0, ty = Fin w}}}
                            , ExprS{rets = ["pr"], expr = BasicExprE{basic_expr = ConstE{val = FinV 0, ty = Fin p}}}
                            , ExprS{rets = ["c"], expr = FunCallE{fname = "Capacity", args = []}}
                            , ForS
                                { loop_ix = "i"
                                , loop_ty = Fin n
                                , loop_body =
                                    SeqS
                                      [ ExprS{rets = ["xi"], expr = BasicExprE{basic_expr = DynIndexE{arr_expr = VarE{var = "xs"}, ix_expr = VarE{var = "i"}}}}
                                      , ExprS{rets = ["y"], expr = RandomSampleE{distr_expr = BernoulliE{prob_one = bernoulli_prob}}}
                                      , ExprS{rets = ["try_pick"], expr = BasicExprE{basic_expr = BinOpE{bin_op = XorOp, lhs = VarE{var = "xi"}, rhs = VarE{var = "y"}}}}
                                      , ExprS{rets = ["wi"], expr = FunCallE{fname = "Weight", args = ["i"]}}
                                      , ExprS{rets = ["wt_picked"], expr = BasicExprE{basic_expr = BinOpE{bin_op = AddOp, lhs = VarE{var = "wt"}, rhs = VarE{var = "wi"}}}}
                                      , ExprS{rets = ["can_fit"], expr = BasicExprE{basic_expr = BinOpE{bin_op = LEqOp, lhs = VarE{var = "wt_picked"}, rhs = VarE{var = "c"}}}}
                                      , ExprS{rets = ["should_pick"], expr = BasicExprE{basic_expr = BinOpE{bin_op = AndOp, lhs = VarE{var = "try_pick"}, rhs = VarE{var = "can_fit"}}}}
                                      , IfThenElseS
                                          { cond = "should_pick"
                                          , s_true =
                                              SeqS
                                                [ ExprS{rets = ["xs"], expr = BasicExprE{basic_expr = UpdateArrE{arr_expr = VarE{var = "xs"}, ix_expr = VarE{var = "i"}, rhs = VarE{var = "should_pick"}}}}
                                                , ExprS{rets = ["wt"], expr = BasicExprE{basic_expr = VarE{var = "wt_picked"}}}
                                                , ExprS{rets = ["pi"], expr = FunCallE{fname = "Profit", args = ["i"]}}
                                                , ExprS{rets = ["pr"], expr = BasicExprE{basic_expr = BinOpE{bin_op = AddOp, lhs = VarE{var = "pr"}, rhs = VarE{var = "pi"}}}}
                                                ]
                                          , s_false = SeqS []
                                          }
                                      ]
                                }
                            , ExprS{rets = ["ok"], expr = BasicExprE{basic_expr = BinOpE{bin_op = LtOp, lhs = VarE{var = "old_pr"}, rhs = VarE{var = "pr"}}}}
                            ]
                      }
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
                    FunBody
                      { param_names = []
                      , ret_names = ["xs"]
                      , body_stmt =
                          SeqS
                            [ ExprS{rets = ["xs"], expr = BasicExprE{basic_expr = DefaultE{ty = Arr n (Fin 2)}}}
                            , ForS
                                { loop_ix = "iter"
                                , loop_ty = Fin k
                                , loop_body =
                                    SeqS
                                      [ ExprS{rets = ["pr"], expr = FunCallE{fname = "TotalProfit", args = ["xs"]}}
                                      , ExprS
                                          { rets = ["ok", "xs'"]
                                          , expr =
                                              PrimCallE
                                                { prim =
                                                    Primitive
                                                      [PartialFun{pfun_name = "TreeGen", pfun_args = [Just "xs", Just "pr"]}]
                                                      (inject (QAmplify @size (Amplify{p_min = bernoulli_prob ** sizeToPrec n})))
                                                }
                                          }
                                      , ExprS{rets = ["xs"], expr = BasicExprE{basic_expr = TernaryE{branch = VarE{var = "ok"}, lhs = VarE{var = "xs'"}, rhs = VarE{var = "xs"}}}}
                                      ]
                                }
                            ]
                      }
              }
        }
    ]
