import random
import numpy as np
import qiskit
import qiskit.circuit
from qiskit.circuit import ClassicalRegister, QuantumRegister

# ===CODE-HERE===


def cli():
    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument("--draw", type=str, metavar="FILE", help="Render the main circuit to a PDF file")
    parser.add_argument("--qasm", action="store_true", help="Output OpenQASM 3 for the main circuit")
    args = parser.parse_args()

    qc = main()

    if args.draw:
        qc.draw("mpl", filename=args.draw)

    if args.qasm:
        from qiskit.qasm3 import dumps

        print(dumps(qc))


if __name__ == "__main__":
    cli()
