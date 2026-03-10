import random
import attrs
import numpy as np

import qualtran as qlt
import qualtran.bloqs.basic_gates as qlt_gates
import qualtran.bloqs.arithmetic as qlt_arith
from qualtran.bloqs.qft.qft_text_book import QFTTextBook



def add_bloq(bb: qlt.BloqBuilder, bloq: qlt.Bloq, regs: list[qlt.SoquetT]):
    reg_names = [r.name for r in bloq.signature]
    return bb.add(bloq, **dict(zip(reg_names, regs)))


@attrs.frozen
class MultiSwap(qlt.Bloq):
    regs: tuple[qlt.Register, ...] = attrs.field(converter=tuple)

    def __attrs_post_init__(self):
        assert len(self.regs) % 2 == 0, "MultiSwap requires an even number of registers"

    @property
    def signature(self):
        return qlt.Signature(list(self.regs))

    def build_composite_bloq(self, bb, **soqs):
        n = len(self.regs)
        half = n // 2
        names = [r.name for r in self.regs]
        for i in range(half):
            a, b = names[i], names[half + i]
            soqs[a], soqs[b] = bb.add(
                qlt_gates.Swap(self.regs[i].bitsize), x=soqs[a], y=soqs[b]
            )
        return soqs


@attrs.frozen
class MultiCopy(qlt.Bloq):
    regs: tuple[qlt.Register, ...] = attrs.field(converter=tuple)

    def __attrs_post_init__(self):
        assert len(self.regs) % 2 == 0, "MultiCopy requires an even number of registers"

    @property
    def signature(self):
        return qlt.Signature(list(self.regs))

    def build_composite_bloq(self, bb, **soqs):
        n = len(self.regs)
        half = n // 2
        names = [r.name for r in self.regs]
        for i in range(half):
            a, b = names[i], names[half + i]
            soqs[a], soqs[b] = bb.add(
                qlt_arith.Xor(self.regs[i].dtype), x=soqs[a], y=soqs[b]
            )
        return soqs

@attrs.frozen
class PhaseOnZero(qlt.Bloq):
    phase: complex
    regs: tuple[qlt.Register, ...] = attrs.field(converter=tuple)

    @property
    def signature(self):
        return qlt.Signature(list(self.regs))



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
        e, e_1 = add_bloq( bb
        , MultiSwap([ qlt.Register("q_1", qlt.BQUInt(1, 2))
        , qlt.Register("q_2", qlt.BQUInt(1, 2)) ])
        , [e, e_1] )
        e, e__1 = add_bloq( bb
        , qlt_gates.XGate().controlled(qlt.CtrlSpec(cvs=0))
        , [e, e__1] )
        e_, e__1 = add_bloq( bb
        , MultiSwap([ qlt.Register("q_1", qlt.BQUInt(1, 2))
        , qlt.Register("q_2", qlt.BQUInt(1, 2)) ])
        , [e_, e__1] )
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
            n_iter[run_ix] = add_bloq(bb, QFTTextBook(2), [n_iter[run_ix]])
            pred_out[run_ix] = add_bloq( bb
            , qlt_gates.XGate()
            , [pred_out[run_ix]] )
            pred_out[run_ix] = add_bloq( bb
            , qlt_gates.Hadamard()
            , [pred_out[run_ix]] )
            s_arg[run_ix] = add_bloq(bb, QFTTextBook(4), [s_arg[run_ix]])
            for LIM in range(3):
                n_iter[run_ix], ctrl[run_ix] = add_bloq( bb
                , qlt_arith.LessThanConstant(2, (LIM - 1))
                , [n_iter[run_ix], ctrl[run_ix]] )
                i, s_arg[run_ix], aux_3, aux, aux_1, aux_2 = add_bloq( bb
                , IsEntryZero_U()
                , [i, s_arg[run_ix], aux_3, aux, aux_1, aux_2] )
                ctrl[run_ix], aux_3, pred_out[run_ix] = add_bloq( bb
                , qlt_gates.Toffoli()
                , [ctrl[run_ix], aux_3, pred_out[run_ix]] )
                i, s_arg[run_ix], aux_3, aux, aux_1, aux_2 = add_bloq( bb
                , IsEntryZero_U().adjoint()
                , [i, s_arg[run_ix], aux_3, aux, aux_1, aux_2] )
                s_arg[run_ix] = add_bloq( bb
                , QFTTextBook(4).adjoint()
                , [s_arg[run_ix]] )
                s_arg[run_ix] = add_bloq( bb
        