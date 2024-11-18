import qiskit
import qiskit.qasm2
import qiskit.qasm3
import sh
from io import StringIO
from icecream import ic


def run_code():
    buf = StringIO()
    sh.runhaskell("experiment.hs", _out=buf)
    qasm = buf.getvalue()

    preface = """
OPENQASM 3.0;
include "stdgates.inc";

opaque reflect_0 q, r;

opaque step(n) q, r;
opaque inv_step(n) q, r;
opaque alg_A q, r;
opaque inv_alg_A q, r;

qubit[10] q;
qubit r;

"""
    qasm = preface + qasm

    try:
        circ = qiskit.qasm3.loads(qasm)
        print(circ)
    except qiskit.qasm3.exceptions.QASM3ImporterError as exc:
        raise ValueError("qasm code:\n" + (">" * 40) + "\n" + qasm + ("<" * 40) + "\n") from exc


def main():
    run_code()


if __name__ == "__main__":
    main()
