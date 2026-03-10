import random
import attrs
import numpy as np

import qualtran as qlt
import qualtran.bloqs.basic_gates as qlt_gates
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
        from qualtran.bloqs.arithmetic.bitwise import Xor

        n = len(self.regs)
        half = n // 2
        names = [r.name for r in self.regs]
        for i in range(half):
            a, b = names[i], names[half + i]
            soqs[a], soqs[b] = bb.add(
                Xor(self.regs[i].dtype), x=soqs[a], y=soqs[b]
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
