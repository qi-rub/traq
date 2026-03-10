import random
import attrs
import numpy as np

import qualtran as qlt
import qualtran.bloqs.basic_gates as qlt_gates
from qualtran.bloqs.qft.qft_text_book import QFTTextBook



def add_bloq(bb: qlt.BloqBuilder, bloq: qlt.Bloq, regs: list[qlt.SoquetT]):
    reg_names = [r.name for r in bloq.signature]
    return bb.add(bloq, **dict(zip(reg_names, regs)))


def bloq_call_and_meas(bloq: qlt.Bloq, *args):
    # convert bloq to cirq circuit
    # initialize input registers in basis state of args
    # run the circuit and measure the first len(args) output registers
    # return the measurement outcomes
    raise NotImplementedError("bloq_call_and_meas")
