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
