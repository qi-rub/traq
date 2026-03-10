import random
import attrs
import numpy as np
import qualtran as qlt

def add_bloq(bb: qlt.BloqBuilder, bloq: qlt.Bloq | str, regs: list[qlt.SoquetT]):
	if bloq == 'swap':
		return reversed(regs)
	if bloq == 'copy':
		raise Exception('TODO')
	reg_names = [r.name for r in bloq.signature]
	return bb.add(bloq, **dict(zip(reg_names, regs)))

def bloq_call_and_meas(bloq: qlt.Bloq, *args):
	# convert bloq to cirq circuit
	# initialize input registers in basis state of args
	# run the circuit and measure the first len(args) output registers
	# return the measurement outcomes
	raise NotImplementedError("bloq_call_and_meas")


@attrs.frozen
class Matrix_U(qlt.Bloq):
    


    @property
    def signature(self):
        return qlt.Signature([ qlt.Register("q_1", qlt.BQUInt(5, 20))
        , qlt.Register("q_2", qlt.BQUInt(4, 10))
        , qlt.Register("q_3", qlt.BQUInt(1, 2)) ])



def Matrix(arg_1 : int, arg_2 : int, arg_3 : int):
    raise Exception('external function - implement here')



@attrs.frozen
class IsEntryZero_U(qlt.Bloq):
    


    @property
    def signature(self):
        return qlt.Signature([ qlt.Register("i0", qlt.BQUInt(5, 20))
        , qlt.Register("j0", qlt.BQUInt(4, 10))
        , qlt.Register("e_", qlt.BQUInt(1, 2))
        , qlt.Register("e", qlt.BQUInt(1, 2))
        , qlt.Register("e_1", qlt.BQUInt(1, 2))
        , qlt.Register("e__1", qlt.BQUInt(1, 2)) ])


    def build_composite_bloq( self
    , bb: qlt.BloqBuilder
    , i0
    , j0
    , e_
    , e
    , e_1
    , e__1 ):
        i0, j0, e_1 = add_bloq(bb, Matrix_U(), [i0, j0, e_1])
        e, e_1 = add_bloq(bb, "swap", [e, e_1])
        e, e__1 = add_bloq(bb, TODO_RevEmbedU, [e, e__1])
        e_, e__1 = add_bloq(bb, "swap", [e_, e__1])
        return {"i0": i0, "j0": j0, "e_": e_, "e": e, "e_1": e_1, "e__1": e__1}



def IsEntryZero(i0 : int, j0 : int, e_ : int):
    i0, j0, e = Matrix(i0, j0, e)
    e_ = not (e)
    return i0, j0, e_


# UAny[Fin 10, 2.6774112591424054e-13]
@attrs.frozen
class UAny(qlt.Bloq):
    


    @property
    def signature(self):
        return qlt.Signature([ qlt.Register("i", qlt.BQUInt(5, 20))
        , qlt.Register("ret", qlt.BQUInt(1, 2))
        , qlt.Register("s_result", qlt.BQUInt(4, 10))
        , qlt.Register("aux", qlt.BQUInt(1, 2))
        , qlt.Register("aux_1", qlt.BQUInt(1, 2))
        , qlt.Register("aux_2", qlt.BQUInt(1, 2))
        , qlt.Register("aux_3", qlt.BQUInt(1, 2))
        , qlt.Register("ctrl", qlt.BQUInt(1, 2), shape=(59))
        , qlt.Register("pred_out", qlt.BQUInt(1, 2), shape=(59))
        , qlt.Register("n_iter", qlt.BQUInt(2, 3), shape=(59))
        , qlt.Register("s_arg", qlt.BQUInt(4, 10), shape=(59)) ])


    def build_composite_bloq( self
    , bb: qlt.BloqBuilder
    , i
    , ret
    , s_result
    , aux
    , aux_1
    , aux_2
    , aux_3
    , ctrl
    , pred_out
    , n_iter
    , s_arg ):
        for run_ix in range(59):
            n_iter[run_ix] = add_bloq(bb, TODO_DistrU, [n_iter[run_ix]])
            pred_out[run_ix] = add_bloq(bb, qlt.XGate(), [pred_out[run_ix]])
            pred_out[run_ix] = add_bloq(bb, qlt.Hadamard(), [pred_out[run_ix]])
            s_arg[run_ix] = add_bloq(bb, TODO_DistrU, [s_arg[run_ix]])
            for LIM in range(3):
                n_iter[run_ix], ctrl[run_ix] = add_bloq( bb
                , TODO_RevEmbedU
                , [n_iter[run_ix], ctrl[run_ix]] )
                i, s_arg[run_ix], aux_3, aux, aux_1, aux_2 = add_bloq( bb
                , IsEntryZero_U()
                , [i, s_arg[run_ix], aux_3, aux, aux_1, aux_2] )
                ctrl[run_ix], aux_3, pred_out[run_ix] = add_bloq( bb
                , qlt.Toffoli()
                , [ctrl[run_ix], aux_3, pred_out[run_ix]] )
                i, s_arg[run_ix], aux_3, aux, aux_1, aux_2 = add_bloq( bb
                , IsEntryZero_U().adjoint()
                , [i, s_arg[run_ix], aux_3, aux, aux_1, aux_2] )
                s_arg[run_ix] = add_bloq( bb
                , TODO_DistrU.adjoint()
                , [s_arg[run_ix]] )
                s_arg[run_ix] = add_bloq(bb, TODO_PhaseOnZero, [s_arg[run_ix]])
                s_arg[run_ix] = add_bloq(bb, TODO_DistrU, [s_arg[run_ix]])
                n_iter[run_ix], ctrl[run_ix] = add_bloq( bb
                , TODO_RevEmbedU
                , [n_iter[run_ix], ctrl[run_ix]] )
            pred_out[run_ix] = add_bloq(bb, qlt.Hadamard(), [pred_out[run_ix]])
            pred_out[run_ix] = add_bloq(bb, qlt.XGate(), [pred_out[run_ix]])
            n_iter[run_ix] = add_bloq( bb
            , TODO_DistrU.adjoint()
            , [n_iter[run_ix]] )
            ctrl[run_ix] = add_bloq(bb, qlt.XGate(), [ctrl[run_ix]])
            i, s_arg[run_ix], aux_3, aux, aux_1, aux_2 = add_bloq( bb
            , IsEntryZero_U()
            , [i, s_arg[run_ix], aux_3, aux, aux_1, aux_2] )
            ctrl[run_ix], aux_3, pred_out[run_ix] = add_bloq( bb
            , qlt.Toffoli()
            , [ctrl[run_ix], aux_3, pred_out[run_ix]] )
            i, s_arg[run_ix], aux_3, aux, aux_1, aux_2 = add_bloq( bb
            , IsEntryZero_U().adjoint()
            , [i, s_arg[run_ix], aux_3, aux, aux_1, aux_2] )
            ctrl[run_ix] = add_bloq(bb, qlt.XGate(), [ctrl[run_ix]])
        pred_out, ret = add_bloq(bb, TODO_RevEmbedU, [pred_out, ret])
        s_arg, pred_out, s_result = add_bloq( bb
        , TODO_RevEmbedU
        , [s_arg, pred_out, s_result] )
        return {"i": i, "ret": ret, "s_result": s_result, "aux": aux, "aux_1": aux_1, "aux_2": aux_2, "aux_3": aux_3, "ctrl": ctrl, "pred_out": pred_out, "n_iter": n_iter, "s_arg": s_arg}



@attrs.frozen
class IsRowAllOnes_U(qlt.Bloq):
    


    @property
    def signature(self):
        return qlt.Signature([ qlt.Register("i", qlt.BQUInt(5, 20))
        , qlt.Register("okr", qlt.BQUInt(1, 2))
        , qlt.Register("hasZero", qlt.BQUInt(1, 2))
        , qlt.Register("hasZero_1", qlt.BQUInt(1, 2))
        , qlt.Register("s_result", qlt.BQUInt(4, 10))
        , qlt.Register("aux", qlt.BQUInt(1, 2))
        , qlt.Register("aux_1", qlt.BQUInt(1, 2))
        , qlt.Register("aux_2", qlt.BQUInt(1, 2))
        , qlt.Register("aux_3", qlt.BQUInt(1, 2))
        , qlt.Register("ctrl", qlt.BQUInt(1, 2), shape=(59))
        , qlt.Register("pred_out", qlt.BQUInt(1, 2), shape=(59))
        , qlt.Register("n_iter", qlt.BQUInt(2, 3), shape=(59))
        , qlt.Register("s_arg", qlt.BQUInt(4, 10), shape=(59))
        , qlt.Register("aux_prim", qlt.BQUInt(4, 10))
        , qlt.Register("aux_prim_1", qlt.BQUInt(1, 2))
        , qlt.Register("aux_prim_2", qlt.BQUInt(1, 2))
        , qlt.Register("aux_prim_3", qlt.BQUInt(1, 2))
        , qlt.Register("aux_prim_4", qlt.BQUInt(1, 2))
        , qlt.Register("aux_prim_5", qlt.BQUInt(1, 2), shape=(59))
        , qlt.Register("aux_prim_6", qlt.BQUInt(1, 2), shape=(59))
        , qlt.Register("aux_prim_7", qlt.BQUInt(2, 3), shape=(59))
        , qlt.Register("aux_prim_8", qlt.BQUInt(4, 10), shape=(59))
        , qlt.Register("okr_1", qlt.BQUInt(1, 2)) ])


    def build_composite_bloq( self
    , bb: qlt.BloqBuilder
    , i
    , okr
    , hasZero
    , hasZero_1
    , s_result
    , aux
    , aux_1
    , aux_2
    , aux_3
    , ctrl
    , pred_out
    , n_iter
    , s_arg
    , aux_prim
    , aux_prim_1
    , aux_prim_2
    , aux_prim_3
    , aux_prim_4
    , aux_prim_5
    , aux_prim_6
    , aux_prim_7
    , aux_prim_8
    , okr_1 ):
        i, hasZero_1, aux_prim, aux_prim_1, aux_prim_2, aux_prim_3, aux_prim_4, aux_prim_5, aux_prim_6, aux_prim_7, aux_prim_8 = add_bloq( bb
        , UAny()
        , [ i
        , hasZero_1
        , aux_prim
        , aux_prim_1
        , aux_prim_2
        , aux_prim_3
        , aux_prim_4
        , aux_prim_5
        , aux_prim_6
        , aux_prim_7
        , aux_prim_8 ] )
        hasZero, hasZero_1 = add_bloq(bb, "swap", [hasZero, hasZero_1])
        hasZero, okr_1 = add_bloq(bb, TODO_RevEmbedU, [hasZero, okr_1])
        okr, okr_1 = add_bloq(bb, "swap", [okr, okr_1])
        return {"i": i, "okr": okr, "hasZero": hasZero, "hasZero_1": hasZero_1, "s_result": s_result, "aux": aux, "aux_1": aux_1, "aux_2": aux_2, "aux_3": aux_3, "ctrl": ctrl, "pred_out": pred_out, "n_iter": n_iter, "s_arg": s_arg, "aux_prim": aux_prim, "aux_prim_1": aux_prim_1, "aux_prim_2": aux_prim_2, "aux_prim_3": aux_prim_3, "aux_prim_4": aux_prim_4, "aux_prim_5": aux_prim_5, "aux_prim_6": aux_prim_6, "aux_prim_7": aux_prim_7, "aux_prim_8": aux_prim_8, "okr_1": okr_1}


# Grover[...]
@attrs.frozen
class Grover(qlt.Bloq):
    k: int


    @property
    def signature(self):
        return qlt.Signature([ qlt.Register("i", qlt.BQUInt(5, 20))
        , qlt.Register("x", qlt.BQUInt(4, 10))
        , qlt.Register("ret_1", qlt.BQUInt(1, 2))
        , qlt.Register("aux_4", qlt.BQUInt(1, 2))
        , qlt.Register("aux_5", qlt.BQUInt(1, 2))
        , qlt.Register("aux_6", qlt.BQUInt(1, 2)) ])


    def build_composite_bloq( self
    , bb: qlt.BloqBuilder
    , i
    , x
    , ret_1
    , aux_4
    , aux_5
    , aux_6 ):
        ret_1 = add_bloq(bb, qlt.XGate(), [ret_1])
        ret_1 = add_bloq(bb, qlt.Hadamard(), [ret_1])
        x = add_bloq(bb, TODO_DistrU, [x])
        for _ in range(k):
            i, x, ret_1, aux_4, aux_5, aux_6 = add_bloq( bb
            , IsEntryZero_U()
            , [i, x, ret_1, aux_4, aux_5, aux_6] )
            x = add_bloq(bb, TODO_DistrU.adjoint(), [x])
            x = add_bloq(bb, TODO_PhaseOnZero, [x])
            x = add_bloq(bb, TODO_DistrU, [x])
        ret_1 = add_bloq(bb, qlt.Hadamard(), [ret_1])
        ret_1 = add_bloq(bb, qlt.XGate(), [ret_1])
        return {"i": i, "x": x, "ret_1": ret_1, "aux_4": aux_4, "aux_5": aux_5, "aux_6": aux_6}



def QAny(i : int, ret_1 : int):
    for _ in range(27):
        Q_sum = 0
        for j_lim in [1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3]:
            j = random.randrange(j_lim)
            Q_sum = (Q_sum + j)
            not_done = (not_done and (Q_sum <= j_lim))
            if (not_done):
                i, s_result_1, ret_1 = bloq_call_and_meas( Grover(j)
                , i
                , s_result_1
                , ret_1 )
                i, s_result_1, ret_1 = bloq_call_and_meas( IsEntryZero_U()
                , i
                , s_result_1
                , ret_1 )
                not_done = (not_done and ret_1)
            else:
                pass
    return i, ret_1



def IsRowAllOnes(i : int, okr : int):
    i, hasZero = QAny(i, hasZero)
    okr = not (hasZero)
    return i, okr


# UAny[Fin 20, 5.0e-4]
@attrs.frozen
class UAny_1(qlt.Bloq):
    


    @property
    def signature(self):
        return qlt.Signature([ qlt.Register("ret_2", qlt.BQUInt(1, 2))
        , qlt.Register("s_result_2", qlt.BQUInt(5, 20))
        , qlt.Register("aux_7", qlt.BQUInt(1, 2))
        , qlt.Register("aux_8", qlt.BQUInt(1, 2))
        , qlt.Register("aux_9", qlt.BQUInt(4, 10))
        , qlt.Register("aux_10", qlt.BQUInt(1, 2))
        , qlt.Register("aux_11", qlt.BQUInt(1, 2))
        , qlt.Register("aux_12", qlt.BQUInt(1, 2))
        , qlt.Register("aux_13", qlt.BQUInt(1, 2))
        , qlt.Register("aux_14", qlt.BQUInt(1, 2), shape=(59))
        , qlt.Register("aux_15", qlt.BQUInt(1, 2), shape=(59))
        , qlt.Register("aux_16", qlt.BQUInt(2, 3), shape=(59))
        , qlt.Register("aux_17", qlt.BQUInt(4, 10), shape=(59))
        , qlt.Register("aux_18", qlt.BQUInt(4, 10))
        , qlt.Register("aux_19", qlt.BQUInt(1, 2))
        , qlt.Register("aux_20", qlt.BQUInt(1, 2))
        , qlt.Register("aux_21", qlt.BQUInt(1, 2))
        , qlt.Register("aux_22", qlt.BQUInt(1, 2))
        , qlt.Register("aux_23", qlt.BQUInt(1, 2), shape=(59))
        , qlt.Register("aux_24", qlt.BQUInt(1, 2), shape=(59))
        , qlt.Register("aux_25", qlt.BQUInt(2, 3), shape=(59))
        , qlt.Register("aux_26", qlt.BQUInt(4, 10), shape=(59))
        , qlt.Register("aux_27", qlt.BQUInt(1, 2))
        , qlt.Register("aux_28", qlt.BQUInt(1, 2))
        , qlt.Register("ctrl_1", qlt.BQUInt(1, 2), shape=(16))
        , qlt.Register("pred_out_1", qlt.BQUInt(1, 2), shape=(16))
        , qlt.Register("n_iter_1", qlt.BQUInt(2, 4), shape=(16))
        , qlt.Register("s_arg_1", qlt.BQUInt(5, 20), shape=(16)) ])


    def build_composite_bloq( self
    , bb: qlt.BloqBuilder
    , ret_2
    , s_result_2
    , aux_7
    , aux_8
    , aux_9
    , aux_10
    , aux_11
    , aux_12
    , aux_13
    , aux_14
    , aux_15
    , aux_16
    , aux_17
    , aux_18
    , aux_19
    , aux_20
    , aux_21
    , aux_22
    , aux_23
    , aux_24
    , aux_25
    , aux_26
    , aux_27
    , aux_28
    , ctrl_1
    , pred_out_1
    , n_iter_1
    , s_arg_1 ):
        for run_ix in range(16):
            n_iter_1[run_ix] = add_bloq(bb, TODO_DistrU, [n_iter_1[run_ix]])
            pred_out_1[run_ix] = add_bloq(bb, qlt.XGate(), [pred_out_1[run_ix]])
            pred_out_1[run_ix] = add_bloq( bb
            , qlt.Hadamard()
            , [pred_out_1[run_ix]] )
            s_arg_1[run_ix] = add_bloq(bb, TODO_DistrU, [s_arg_1[run_ix]])
            for LIM in range(4):
                n_iter_1[run_ix], ctrl_1[run_ix] = add_bloq( bb
                , TODO_RevEmbedU
                , [n_iter_1[run_ix], ctrl_1[run_ix]] )
                s_arg_1[run_ix], aux_28, aux_7, aux_8, aux_9, aux_10, aux_11, aux_12, aux_13, aux_14, aux_15, aux_16, aux_17, aux_18, aux_19, aux_20, aux_21, aux_22, aux_23, aux_24, aux_25, aux_26, aux_27 = add_bloq( bb
                , IsRowAllOnes_U()
                , [ s_arg_1[run_ix]
                , aux_28
                , aux_7
                , aux_8
                , aux_9
                , aux_10
                , aux_11
                , aux_12
                , aux_13
                , aux_14
                , aux_15
                , aux_16
                , aux_17
                , aux_18
                , aux_19
                , aux_20
                , aux_21
                , aux_22
                , aux_23
                , aux_24
                , aux_25
                , aux_26
                , aux_27 ] )
                ctrl_1[run_ix], aux_28, pred_out_1[run_ix] = add_bloq( bb
                , qlt.Toffoli()
                , [ctrl_1[run_ix], aux_28, pred_out_1[run_ix]] )
                s_arg_1[run_ix], aux_28, aux_7, aux_8, aux_9, aux_10, aux_11, aux_12, aux_13, aux_14, aux_15, aux_16, aux_17, aux_18, aux_19, aux_20, aux_21, aux_22, aux_23, aux_24, aux_25, aux_26, aux_27 = add_bloq( bb
                , IsRowAllOnes_U().adjoint()
                , [ s_arg_1[run_ix]
                , aux_28
                , aux_7
                , aux_8
                , aux_9
                , aux_10
                , aux_11
                , aux_12
                , aux_13
                , aux_14
                , aux_15
                , aux_16
                , aux_17
                , aux_18
                , aux_19
                , aux_20
                , aux_21
                , aux_22
                , aux_23
                , aux_24
                , aux_25
                , aux_26
                , aux_27 ] )
                s_arg_1[run_ix] = add_bloq( bb
                , TODO_DistrU.adjoint()
                , [s_arg_1[run_ix]] )
                s_arg_1[run_ix] = add_bloq( bb
                , TODO_PhaseOnZero
                , [s_arg_1[run_ix]] )
                s_arg_1[run_ix] = add_bloq(bb, TODO_DistrU, [s_arg_1[run_ix]])
                n_iter_1[run_ix], ctrl_1[run_ix] = add_bloq( bb
                , TODO_RevEmbedU
                , [n_iter_1[run_ix], ctrl_1[run_ix]] )
            pred_out_1[run_ix] = add_bloq( bb
            , qlt.Hadamard()
            , [pred_out_1[run_ix]] )
            pred_out_1[run_ix] = add_bloq(bb, qlt.XGate(), [pred_out_1[run_ix]])
            n_iter_1[run_ix] = add_bloq( bb
            , TODO_DistrU.adjoint()
            , [n_iter_1[run_ix]] )
            ctrl_1[run_ix] = add_bloq(bb, qlt.XGate(), [ctrl_1[run_ix]])
            s_arg_1[run_ix], aux_28, aux_7, aux_8, aux_9, aux_10, aux_11, aux_12, aux_13, aux_14, aux_15, aux_16, aux_17, aux_18, aux_19, aux_20, aux_21, aux_22, aux_23, aux_24, aux_25, aux_26, aux_27 = add_bloq( bb
            , IsRowAllOnes_U()
            , [ s_arg_1[run_ix]
            , aux_28
            , aux_7
            , aux_8
            , aux_9
            , aux_10
            , aux_11
            , aux_12
            , aux_13
            , aux_14
            , aux_15
            , aux_16
            , aux_17
            , aux_18
            , aux_19
            , aux_20
            , aux_21
            , aux_22
            , aux_23
            , aux_24
            , aux_25
            , aux_26
            , aux_27 ] )
            ctrl_1[run_ix], aux_28, pred_out_1[run_ix] = add_bloq( bb
            , qlt.Toffoli()
            , [ctrl_1[run_ix], aux_28, pred_out_1[run_ix]] )
            s_arg_1[run_ix], aux_28, aux_7, aux_8, aux_9, aux_10, aux_11, aux_12, aux_13, aux_14, aux_15, aux_16, aux_17, aux_18, aux_19, aux_20, aux_21, aux_22, aux_23, aux_24, aux_25, aux_26, aux_27 = add_bloq( bb
            , IsRowAllOnes_U().adjoint()
            , [ s_arg_1[run_ix]
            , aux_28
            , aux_7
            , aux_8
            , aux_9
            , aux_10
            , aux_11
            , aux_12
            , aux_13
            , aux_14
            , aux_15
            , aux_16
            , aux_17
            , aux_18
            , aux_19
            , aux_20
            , aux_21
            , aux_22
            , aux_23
            , aux_24
            , aux_25
            , aux_26
            , aux_27 ] )
            ctrl_1[run_ix] = add_bloq(bb, qlt.XGate(), [ctrl_1[run_ix]])
        pred_out_1, ret_2 = add_bloq(bb, TODO_RevEmbedU, [pred_out_1, ret_2])
        s_arg_1, pred_out_1, s_result_2 = add_bloq( bb
        , TODO_RevEmbedU
        , [s_arg_1, pred_out_1, s_result_2] )
        return {"ret_2": ret_2, "s_result_2": s_result_2, "aux_7": aux_7, "aux_8": aux_8, "aux_9": aux_9, "aux_10": aux_10, "aux_11": aux_11, "aux_12": aux_12, "aux_13": aux_13, "aux_14": aux_14, "aux_15": aux_15, "aux_16": aux_16, "aux_17": aux_17, "aux_18": aux_18, "aux_19": aux_19, "aux_20": aux_20, "aux_21": aux_21, "aux_22": aux_22, "aux_23": aux_23, "aux_24": aux_24, "aux_25": aux_25, "aux_26": aux_26, "aux_27": aux_27, "aux_28": aux_28, "ctrl_1": ctrl_1, "pred_out_1": pred_out_1, "n_iter_1": n_iter_1, "s_arg_1": s_arg_1}



@attrs.frozen
class HasAllOnesRow_U(qlt.Bloq):
    


    @property
    def signature(self):
        return qlt.Signature([ qlt.Register("ok", qlt.BQUInt(1, 2))
        , qlt.Register("ok_1", qlt.BQUInt(1, 2))
        , qlt.Register("s_result_2", qlt.BQUInt(5, 20))
        , qlt.Register("aux_7", qlt.BQUInt(1, 2))
        , qlt.Register("aux_8", qlt.BQUInt(1, 2))
        , qlt.Register("aux_9", qlt.BQUInt(4, 10))
        , qlt.Register("aux_10", qlt.BQUInt(1, 2))
        , qlt.Register("aux_11", qlt.BQUInt(1, 2))
        , qlt.Register("aux_12", qlt.BQUInt(1, 2))
        , qlt.Register("aux_13", qlt.BQUInt(1, 2))
        , qlt.Register("aux_14", qlt.BQUInt(1, 2), shape=(59))
        , qlt.Register("aux_15", qlt.BQUInt(1, 2), shape=(59))
        , qlt.Register("aux_16", qlt.BQUInt(2, 3), shape=(59))
        , qlt.Register("aux_17", qlt.BQUInt(4, 10), shape=(59))
        , qlt.Register("aux_18", qlt.BQUInt(4, 10))
        , qlt.Register("aux_19", qlt.BQUInt(1, 2))
        , qlt.Register("aux_20", qlt.BQUInt(1, 2))
        , qlt.Register("aux_21", qlt.BQUInt(1, 2))
        , qlt.Register("aux_22", qlt.BQUInt(1, 2))
        , qlt.Register("aux_23", qlt.BQUInt(1, 2), shape=(59))
        , qlt.Register("aux_24", qlt.BQUInt(1, 2), shape=(59))
        , qlt.Register("aux_25", qlt.BQUInt(2, 3), shape=(59))
        , qlt.Register("aux_26", qlt.BQUInt(4, 10), shape=(59))
        , qlt.Register("aux_27", qlt.BQUInt(1, 2))
        , qlt.Register("aux_28", qlt.BQUInt(1, 2))
        , qlt.Register("ctrl_1", qlt.BQUInt(1, 2), shape=(16))
        , qlt.Register("pred_out_1", qlt.BQUInt(1, 2), shape=(16))
        , qlt.Register("n_iter_1", qlt.BQUInt(2, 4), shape=(16))
        , qlt.Register("s_arg_1", qlt.BQUInt(5, 20), shape=(16))
        , qlt.Register("aux_prim_9", qlt.BQUInt(5, 20))
        , qlt.Register("aux_prim_10", qlt.BQUInt(1, 2))
        , qlt.Register("aux_prim_11", qlt.BQUInt(1, 2))
        , qlt.Register("aux_prim_12", qlt.BQUInt(4, 10))
        , qlt.Register("aux_prim_13", qlt.BQUInt(1, 2))
        , qlt.Register("aux_prim_14", qlt.BQUInt(1, 2))
        , qlt.Register("aux_prim_15", qlt.BQUInt(1, 2))
        , qlt.Register("aux_prim_16", qlt.BQUInt(1, 2))
        , qlt.Register("aux_prim_17", qlt.BQUInt(1, 2), shape=(59))
        , qlt.Register("aux_prim_18", qlt.BQUInt(1, 2), shape=(59))
        , qlt.Register("aux_prim_19", qlt.BQUInt(2, 3), shape=(59))
        , qlt.Register("aux_prim_20", qlt.BQUInt(4, 10), shape=(59))
        , qlt.Register("aux_prim_21", qlt.BQUInt(4, 10))
        , qlt.Register("aux_prim_22", qlt.BQUInt(1, 2))
        , qlt.Register("aux_prim_23", qlt.BQUInt(1, 2))
        , qlt.Register("aux_prim_24", qlt.BQUInt(1, 2))
        , qlt.Register("aux_prim_25", qlt.BQUInt(1, 2))
        , qlt.Register("aux_prim_26", qlt.BQUInt(1, 2), shape=(59))
        , qlt.Register("aux_prim_27", qlt.BQUInt(1, 2), shape=(59))
        , qlt.Register("aux_prim_28", qlt.BQUInt(2, 3), shape=(59))
        , qlt.Register("aux_prim_29", qlt.BQUInt(4, 10), shape=(59))
        , qlt.Register("aux_prim_30", qlt.BQUInt(1, 2))
        , qlt.Register("aux_prim_31", qlt.BQUInt(1, 2))
        , qlt.Register("aux_prim_32", qlt.BQUInt(1, 2), shape=(16))
        , qlt.Register("aux_prim_33", qlt.BQUInt(1, 2), shape=(16))
        , qlt.Register("aux_prim_34", qlt.BQUInt(2, 4), shape=(16))
        , qlt.Register("aux_prim_35", qlt.BQUInt(5, 20), shape=(16)) ])


    def build_composite_bloq( self
    , bb: qlt.BloqBuilder
    , ok
    , ok_1
    , s_result_2
    , aux_7
    , aux_8
    , aux_9
    , aux_10
    , aux_11
    , aux_12
    , aux_13
    , aux_14
    , aux_15
    , aux_16
    , aux_17
    , aux_18
    , aux_19
    , aux_20
    , aux_21
    , aux_22
    , aux_23
    , aux_24
    , aux_25
    , aux_26
    , aux_27
    , aux_28
    , ctrl_1
    , pred_out_1
    , n_iter_1
    , s_arg_1
    , aux_prim_9
    , aux_prim_10
    , aux_prim_11
    , aux_prim_12
    , aux_prim_13
    , aux_prim_14
    , aux_prim_15
    , aux_prim_16
    , aux_prim_17
    , aux_prim_18
    , aux_prim_19
    , aux_prim_20
    , aux_prim_21
    , aux_prim_22
    , aux_prim_23
    , aux_prim_24
    , aux_prim_25
    , aux_prim_26
    , aux_prim_27
    , aux_prim_28
    , aux_prim_29
    , aux_prim_30
    , aux_prim_31
    , aux_prim_32
    , aux_prim_33
    , aux_prim_34
    , aux_prim_35 ):
        ok_1, aux_prim_9, aux_prim_10, aux_prim_11, aux_prim_12, aux_prim_13, aux_prim_14, aux_prim_15, aux_prim_16, aux_prim_17, aux_prim_18, aux_prim_19, aux_prim_20, aux_prim_21, aux_prim_22, aux_prim_23, aux_prim_24, aux_prim_25, aux_prim_26, aux_prim_27, aux_prim_28, aux_prim_29, aux_prim_30, aux_prim_31, aux_prim_32, aux_prim_33, aux_prim_34, aux_prim_35 = add_bloq( bb
        , UAny_1()
        , [ ok_1
        , aux_prim_9
        , aux_prim_10
        , aux_prim_11
        , aux_prim_12
        , aux_prim_13
        , aux_prim_14
        , aux_prim_15
        , aux_prim_16
        , aux_prim_17
        , aux_prim_18
        , aux_prim_19
        , aux_prim_20
        , aux_prim_21
        , aux_prim_22
        , aux_prim_23
        , aux_prim_24
        , aux_prim_25
        , aux_prim_26
        , aux_prim_27
        , aux_prim_28
        , aux_prim_29
        , aux_prim_30
        , aux_prim_31
        , aux_prim_32
        , aux_prim_33
        , aux_prim_34
        , aux_prim_35 ] )
        ok, ok_1 = add_bloq(bb, "swap", [ok, ok_1])
        return {"ok": ok, "ok_1": ok_1, "s_result_2": s_result_2, "aux_7": aux_7, "aux_8": aux_8, "aux_9": aux_9, "aux_10": aux_10, "aux_11": aux_11, "aux_12": aux_12, "aux_13": aux_13, "aux_14": aux_14, "aux_15": aux_15, "aux_16": aux_16, "aux_17": aux_17, "aux_18": aux_18, "aux_19": aux_19, "aux_20": aux_20, "aux_21": aux_21, "aux_22": aux_22, "aux_23": aux_23, "aux_24": aux_24, "aux_25": aux_25, "aux_26": aux_26, "aux_27": aux_27, "aux_28": aux_28, "ctrl_1": ctrl_1, "pred_out_1": pred_out_1, "n_iter_1": n_iter_1, "s_arg_1": s_arg_1, "aux_prim_9": aux_prim_9, "aux_prim_10": aux_prim_10, "aux_prim_11": aux_prim_11, "aux_prim_12": aux_prim_12, "aux_prim_13": aux_prim_13, "aux_prim_14": aux_prim_14, "aux_prim_15": aux_prim_15, "aux_prim_16": aux_prim_16, "aux_prim_17": aux_prim_17, "aux_prim_18": aux_prim_18, "aux_prim_19": aux_prim_19, "aux_prim_20": aux_prim_20, "aux_prim_21": aux_prim_21, "aux_prim_22": aux_prim_22, "aux_prim_23": aux_prim_23, "aux_prim_24": aux_prim_24, "aux_prim_25": aux_prim_25, "aux_prim_26": aux_prim_26, "aux_prim_27": aux_prim_27, "aux_prim_28": aux_prim_28, "aux_prim_29": aux_prim_29, "aux_prim_30": aux_prim_30, "aux_prim_31": aux_prim_31, "aux_prim_32": aux_prim_32, "aux_prim_33": aux_prim_33, "aux_prim_34": aux_prim_34, "aux_prim_35": aux_prim_35}


# Grover[...]
@attrs.frozen
class Grover_1(qlt.Bloq):
    k: int


    @property
    def signature(self):
        return qlt.Signature([ qlt.Register("x_1", qlt.BQUInt(5, 20))
        , qlt.Register("ret_3", qlt.BQUInt(1, 2))
        , qlt.Register("aux_29", qlt.BQUInt(1, 2))
        , qlt.Register("aux_30", qlt.BQUInt(1, 2))
        , qlt.Register("aux_31", qlt.BQUInt(4, 10))
        , qlt.Register("aux_32", qlt.BQUInt(1, 2))
        , qlt.Register("aux_33", qlt.BQUInt(1, 2))
        , qlt.Register("aux_34", qlt.BQUInt(1, 2))
        , qlt.Register("aux_35", qlt.BQUInt(1, 2))
        , qlt.Register("aux_36", qlt.BQUInt(1, 2), shape=(59))
        , qlt.Register("aux_37", qlt.BQUInt(1, 2), shape=(59))
        , qlt.Register("aux_38", qlt.BQUInt(2, 3), shape=(59))
        , qlt.Register("aux_39", qlt.BQUInt(4, 10), shape=(59))
        , qlt.Register("aux_40", qlt.BQUInt(4, 10))
        , qlt.Register("aux_41", qlt.BQUInt(1, 2))
        , qlt.Register("aux_42", qlt.BQUInt(1, 2))
        , qlt.Register("aux_43", qlt.BQUInt(1, 2))
        , qlt.Register("aux_44", qlt.BQUInt(1, 2))
        , qlt.Register("aux_45", qlt.BQUInt(1, 2), shape=(59))
        , qlt.Register("aux_46", qlt.BQUInt(1, 2), shape=(59))
        , qlt.Register("aux_47", qlt.BQUInt(2, 3), shape=(59))
        , qlt.Register("aux_48", qlt.BQUInt(4, 10), shape=(59))
        , qlt.Register("aux_49", qlt.BQUInt(1, 2)) ])


    def build_composite_bloq( self
    , bb: qlt.BloqBuilder
    , x_1
    , ret_3
    , aux_29
    , aux_30
    , aux_31
    , aux_32
    , aux_33
    , aux_34
    , aux_35
    , aux_36
    , aux_37
    , aux_38
    , aux_39
    , aux_40
    , aux_41
    , aux_42
    , aux_43
    , aux_44
    , aux_45
    , aux_46
    , aux_47
    , aux_48
    , aux_49 ):
        ret_3 = add_bloq(bb, qlt.XGate(), [ret_3])
        ret_3 = add_bloq(bb, qlt.Hadamard(), [ret_3])
        x_1 = add_bloq(bb, TODO_DistrU, [x_1])
        for _ in range(k):
            x_1, ret_3, aux_29, aux_30, aux_31, aux_32, aux_33, aux_34, aux_35, aux_36, aux_37, aux_38, aux_39, aux_40, aux_41, aux_42, aux_43, aux_44, aux_45, aux_46, aux_47, aux_48, aux_49 = add_bloq( bb
            , IsRowAllOnes_U()
            , [ x_1
            , ret_3
            , aux_29
            , aux_30
            , aux_31
            , aux_32
            , aux_33
            , aux_34
            , aux_35
            , aux_36
            , aux_37
            , aux_38
            , aux_39
            , aux_40
            , aux_41
            , aux_42
            , aux_43
            , aux_44
            , aux_45
            , aux_46
            , aux_47
            , aux_48
            , aux_49 ] )
            x_1 = add_bloq(bb, TODO_DistrU.adjoint(), [x_1])
            x_1 = add_bloq(bb, TODO_PhaseOnZero, [x_1])
            x_1 = add_bloq(bb, TODO_DistrU, [x_1])
        ret_3 = add_bloq(bb, qlt.Hadamard(), [ret_3])
        ret_3 = add_bloq(bb, qlt.XGate(), [ret_3])
        return {"x_1": x_1, "ret_3": ret_3, "aux_29": aux_29, "aux_30": aux_30, "aux_31": aux_31, "aux_32": aux_32, "aux_33": aux_33, "aux_34": aux_34, "aux_35": aux_35, "aux_36": aux_36, "aux_37": aux_37, "aux_38": aux_38, "aux_39": aux_39, "aux_40": aux_40, "aux_41": aux_41, "aux_42": aux_42, "aux_43": aux_43, "aux_44": aux_44, "aux_45": aux_45, "aux_46": aux_46, "aux_47": aux_47, "aux_48": aux_48, "aux_49": aux_49}



def QAny_1(ret_3 : int):
    for _ in range(7):
        Q_sum_1 = 0
        for j_lim_1 in [1, 1, 1, 2, 2, 2, 3, 4, 4, 4, 4, 4, 4, 4]:
            j_1 = random.randrange(j_lim_1)
            Q_sum_1 = (Q_sum_1 + j_1)
            not_done_1 = (not_done_1 and (Q_sum_1 <= j_lim_1))
            if (not_done_1):
                s_result_3, ret_3 = bloq_call_and_meas( Grover_1(j_1)
                , s_result_3
                , ret_3 )
                s_result_3, ret_3 = bloq_call_and_meas( IsRowAllOnes_U()
                , s_result_3
                , ret_3 )
                not_done_1 = (not_done_1 and ret_3)
            else:
                pass
    return ret_3



def HasAllOnesRow(ok : int):
    ok = QAny_1(ok)
    return ok

