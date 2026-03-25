import random
import numpy as np
import qiskit
import qiskit.circuit
from qiskit.circuit import ClassicalRegister, QuantumRegister


def Oracle_U():
    q_1 = qiskit.circuit.QuantumRegister(5, "q_1")
    q_2 = qiskit.circuit.QuantumRegister(1, "q_2")
    qc = qiskit.circuit.QuantumCircuit(q_1, q_2, name="Oracle_U")
    return qc


def Oracle():
    c_1 = qiskit.circuit.ClassicalRegister(5, "c_1")
    c_2 = qiskit.circuit.ClassicalRegister(1, "c_2")
    qc = qiskit.circuit.QuantumCircuit(c_1, c_2, name="Oracle")
    return qc


# UAny[Fin 20, 1.0e-2]
def UAny():
    ret = qiskit.circuit.QuantumRegister(1, "ret")
    s_result = qiskit.circuit.QuantumRegister(5, "s_result")
    aux = qiskit.circuit.QuantumRegister(1, "aux")
    ctrl = qiskit.circuit.QuantumRegister(10, "ctrl")
    pred_out = qiskit.circuit.QuantumRegister(10, "pred_out")
    n_iter = qiskit.circuit.QuantumRegister(20, "n_iter")
    s_arg = qiskit.circuit.QuantumRegister(50, "s_arg")

    qc = qiskit.circuit.QuantumCircuit(
        ret, s_result, aux, ctrl, pred_out, n_iter, s_arg, name="UAny"
    )

    qc.append(qiskit.circuit.Gate("UForInRangeS", qc.num_qubits, []), qc.qubits)
    qc.append(
        qiskit.circuit.Gate(
            "RevEmbedU [a] (UnOpE {un_op = AnyOp, operand = VarE {var = a}})", 11, []
        ),
        [*pred_out, *ret],
    )
    qc.append(
        qiskit.circuit.Gate(
            "RevEmbedU [a,f] (BinOpE {bin_op = VecSelectOp, lhs = VarE {var = a}, rhs = VarE {var = f}})",
            65,
            [],
        ),
        [*s_arg, *pred_out, *s_result],
    )

    return qc


def main_U():
    ok = qiskit.circuit.QuantumRegister(1, "ok")
    ok_1 = qiskit.circuit.QuantumRegister(1, "ok_1")
    s_result = qiskit.circuit.QuantumRegister(5, "s_result")
    aux = qiskit.circuit.QuantumRegister(1, "aux")
    ctrl = qiskit.circuit.QuantumRegister(10, "ctrl")
    pred_out = qiskit.circuit.QuantumRegister(10, "pred_out")
    n_iter = qiskit.circuit.QuantumRegister(20, "n_iter")
    s_arg = qiskit.circuit.QuantumRegister(50, "s_arg")
    aux_prim = qiskit.circuit.QuantumRegister(5, "aux_prim")
    aux_prim_1 = qiskit.circuit.QuantumRegister(1, "aux_prim_1")
    aux_prim_2 = qiskit.circuit.QuantumRegister(10, "aux_prim_2")
    aux_prim_3 = qiskit.circuit.QuantumRegister(10, "aux_prim_3")
    aux_prim_4 = qiskit.circuit.QuantumRegister(20, "aux_prim_4")
    aux_prim_5 = qiskit.circuit.QuantumRegister(50, "aux_prim_5")

    qc = qiskit.circuit.QuantumCircuit(
        ok,
        ok_1,
        s_result,
        aux,
        ctrl,
        pred_out,
        n_iter,
        s_arg,
        aux_prim,
        aux_prim_1,
        aux_prim_2,
        aux_prim_3,
        aux_prim_4,
        aux_prim_5,
        name="main_U",
    )

    qc.append(
        UAny().to_gate(),
        [
            *ok_1,
            *aux_prim,
            *aux_prim_1,
            *aux_prim_2,
            *aux_prim_3,
            *aux_prim_4,
            *aux_prim_5,
        ],
    )
    qc.append(qiskit.circuit.library.SwapGate(), [*ok, *ok_1])

    return qc


# Grover[...]
def Grover():
    k = qiskit.circuit.Parameter("k")
    x = qiskit.circuit.QuantumRegister(5, "x")
    ret_1 = qiskit.circuit.QuantumRegister(1, "ret_1")

    qc = qiskit.circuit.QuantumCircuit(x, ret_1, name="Grover")

    qc.append(qiskit.circuit.library.XGate(), [*ret_1])
    qc.append(qiskit.circuit.library.HGate(), [*ret_1])
    qc.append(
        qiskit.circuit.Gate("DistrU (UniformE {sample_ty = Fin 20})", 5, []), [*x]
    )
    with qc.for_loop(range(k)):
        qc.append(Oracle_U().to_gate(), [*x, *ret_1])
        qc.append(
            qiskit.circuit.Gate(
                "DistrU (UniformE {sample_ty = Fin 20})", 5, []
            ).inverse(),
            [*x],
        )
        qc.append(qiskit.circuit.Gate("PhaseOnZero(3.141592653589793)", 5, []), [*x])
        qc.append(
            qiskit.circuit.Gate("DistrU (UniformE {sample_ty = Fin 20})", 5, []), [*x]
        )
    qc.append(qiskit.circuit.library.HGate(), [*ret_1])
    qc.append(qiskit.circuit.library.XGate(), [*ret_1])

    return qc


def QAny():
    ret_1 = qiskit.circuit.ClassicalRegister(1, "ret_1")
    s_result_1 = qiskit.circuit.ClassicalRegister(5, "s_result_1")
    not_done = qiskit.circuit.ClassicalRegister(1, "not_done")
    Q_sum = qiskit.circuit.ClassicalRegister(6, "Q_sum")
    j = qiskit.circuit.ClassicalRegister(6, "j")
    j_lim = qiskit.circuit.ClassicalRegister(6, "j_lim")
    qc = qiskit.circuit.QuantumCircuit(
        ret_1, s_result_1, not_done, Q_sum, j, j_lim, name="QAny"
    )
    for _ in range(5):
        Q_sum = 0
        for j_lim in [1, 1, 1, 2, 2, 2, 3, 4, 4, 4, 4, 4, 4, 4]:
            j = random.randrange(j_lim)
            Q_sum = Q_sum + j
            not_done = not_done and (Q_sum <= j_lim)
            if not_done:
                qc.append(
                    qiskit.circuit.Gate("UProcAndMeas", qc.num_qubits, []), qc.qubits
                )
                qc.append(
                    qiskit.circuit.Gate("UProcAndMeas", qc.num_qubits, []), qc.qubits
                )
                not_done = not_done and ret_1
            else:
                pass
    return qc


def main():
    ok = qiskit.circuit.ClassicalRegister(1, "ok")
    qc = qiskit.circuit.QuantumCircuit(ok, name="main")
    qc.append(QAny().to_instruction(), [], [*ok])
    return qc


def cli():
    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--draw", type=str, metavar="FILE", help="Render the main circuit to a PDF file"
    )
    parser.add_argument(
        "--qasm", action="store_true", help="Output OpenQASM 3 for the main circuit"
    )
    args = parser.parse_args()

    qc = main()

    if args.draw:
        qc.draw("mpl", filename=args.draw)
        print(f"Wrote {args.draw}")

    if args.qasm:
        from qiskit.qasm3 import dumps

        print(dumps(qc))


if __name__ == "__main__":
    cli()
