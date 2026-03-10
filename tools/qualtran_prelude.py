import random
import attrs
import numpy as np
import qualtran as qlt


def add_bloq(bb: qlt.BloqBuilder, bloq: qlt.Bloq | str, regs: list[qlt.SoquetT]):
    if bloq == "swap":
        return reversed(regs)
    if bloq == "copy":
        raise Exception("TODO")
    reg_names = [r.name for r in bloq.signature]
    return bb.add(bloq, **dict(zip(reg_names, regs)))


def bloq_call_and_meas(bloq: qlt.Bloq, *args):
    # convert bloq to cirq circuit
    # initialize input registers in basis state of args
    # run the circuit and measure the first len(args) output registers
    # return the measurement outcomes
    raise NotImplementedError("bloq_call_and_meas")
