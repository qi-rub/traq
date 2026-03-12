import random
import numpy as np
import qiskit
import qiskit.circuit


def Matrix_U():
    q_1 = qiskit.circuit.QuantumRegister(5, "q_1")
    q_2 = qiskit.circuit.QuantumRegister(4, "q_2")
    q_3 = qiskit.circuit.QuantumRegister(1, "q_3")
    qc = qiskit.circuit.QuantumCircuit(q_1, q_2, q_3, name="Matrix_U")
    return qc


def Matrix():
    c_1 = qiskit.circuit.ClassicalRegister(5, "c_1")
    c_2 = qiskit.circuit.ClassicalRegister(4, "c_2")
    c_3 = qiskit.circuit.ClassicalRegister(1, "c_3")
    qc = qiskit.circuit.QuantumCircuit(c_1, c_2, c_3, name="Matrix")
    return qc


def IsEntryZero_U():
    i0 = qiskit.circuit.QuantumRegister(5, "i0")
    j0 = qiskit.circuit.QuantumRegister(4, "j0")
    e_ = qiskit.circuit.QuantumRegister(1, "e_")
    e = qiskit.circuit.QuantumRegister(1, "e")
    e_1 = qiskit.circuit.QuantumRegister(1, "e_1")
    e__1 = qiskit.circuit.QuantumRegister(1, "e__1")
    qc = qiskit.circuit.QuantumCircuit(i0, j0, e_, e, e_1, e__1, name="IsEntryZero_U")
    qc.append(Matrix_U().to_gate(), [*i0, *j0, *e_1])
    qc.append(qiskit.circuit.Gate("BasicGateU SWAP", 2, []), [*e, *e_1])
    qc.append(
        qiskit.circuit.Gate(
            "RevEmbedU [e] (UnOpE {un_op = NotOp, operand = VarE {var = e}})", 2, []
        ),
        [*e, *e__1],
    )
    qc.append(qiskit.circuit.Gate("BasicGateU SWAP", 2, []), [*e_, *e__1])
    return qc


def IsEntryZero():
    i0 = qiskit.circuit.ClassicalRegister(5, "i0")
    j0 = qiskit.circuit.ClassicalRegister(4, "j0")
    e_ = qiskit.circuit.ClassicalRegister(1, "e_")
    e = qiskit.circuit.ClassicalRegister(1, "e")
    qc = qiskit.circuit.QuantumCircuit(i0, j0, e_, e, name="IsEntryZero")
    i0, j0, e = Matrix(i0, j0, e)
    e_ = not (e)
    return qc


# UAny[Fin 10, 2.6774112591424054e-13]
def UAny():
    i = qiskit.circuit.QuantumRegister(5, "i")
    ret = qiskit.circuit.QuantumRegister(1, "ret")
    s_result = qiskit.circuit.QuantumRegister(4, "s_result")
    aux = qiskit.circuit.QuantumRegister(1, "aux")
    aux_1 = qiskit.circuit.QuantumRegister(1, "aux_1")
    aux_2 = qiskit.circuit.QuantumRegister(1, "aux_2")
    aux_3 = qiskit.circuit.QuantumRegister(1, "aux_3")
    ctrl = qiskit.circuit.QuantumRegister(59, "ctrl")
    pred_out = qiskit.circuit.QuantumRegister(59, "pred_out")
    n_iter = qiskit.circuit.QuantumRegister(118, "n_iter")
    s_arg = qiskit.circuit.QuantumRegister(236, "s_arg")
    qc = qiskit.circuit.QuantumCircuit(
        i,
        ret,
        s_result,
        aux,
        aux_1,
        aux_2,
        aux_3,
        ctrl,
        pred_out,
        n_iter,
        s_arg,
        name="UAny",
    )
    qc.append(qiskit.circuit.Gate("UForInRangeS", qc.num_qubits, []), qc.qubits)
    qc.append(
        qiskit.circuit.Gate(
            "RevEmbedU [a] (UnOpE {un_op = AnyOp, operand = VarE {var = a}})", 60, []
        ),
        [*pred_out, *ret],
    )
    qc.append(
        qiskit.circuit.Gate(
            "RevEmbedU [a,f] (BinOpE {bin_op = VecSelectOp, lhs = VarE {var = a}, rhs = VarE {var = f}})",
            299,
            [],
        ),
        [*s_arg, *pred_out, *s_result],
    )
    return qc


def IsRowAllOnes_U():
    i = qiskit.circuit.QuantumRegister(5, "i")
    okr = qiskit.circuit.QuantumRegister(1, "okr")
    hasZero = qiskit.circuit.QuantumRegister(1, "hasZero")
    hasZero_1 = qiskit.circuit.QuantumRegister(1, "hasZero_1")
    s_result = qiskit.circuit.QuantumRegister(4, "s_result")
    aux = qiskit.circuit.QuantumRegister(1, "aux")
    aux_1 = qiskit.circuit.QuantumRegister(1, "aux_1")
    aux_2 = qiskit.circuit.QuantumRegister(1, "aux_2")
    aux_3 = qiskit.circuit.QuantumRegister(1, "aux_3")
    ctrl = qiskit.circuit.QuantumRegister(59, "ctrl")
    pred_out = qiskit.circuit.QuantumRegister(59, "pred_out")
    n_iter = qiskit.circuit.QuantumRegister(118, "n_iter")
    s_arg = qiskit.circuit.QuantumRegister(236, "s_arg")
    aux_prim = qiskit.circuit.QuantumRegister(4, "aux_prim")
    aux_prim_1 = qiskit.circuit.QuantumRegister(1, "aux_prim_1")
    aux_prim_2 = qiskit.circuit.QuantumRegister(1, "aux_prim_2")
    aux_prim_3 = qiskit.circuit.QuantumRegister(1, "aux_prim_3")
    aux_prim_4 = qiskit.circuit.QuantumRegister(1, "aux_prim_4")
    aux_prim_5 = qiskit.circuit.QuantumRegister(59, "aux_prim_5")
    aux_prim_6 = qiskit.circuit.QuantumRegister(59, "aux_prim_6")
    aux_prim_7 = qiskit.circuit.QuantumRegister(118, "aux_prim_7")
    aux_prim_8 = qiskit.circuit.QuantumRegister(236, "aux_prim_8")
    okr_1 = qiskit.circuit.QuantumRegister(1, "okr_1")
    qc = qiskit.circuit.QuantumCircuit(
        i,
        okr,
        hasZero,
        hasZero_1,
        s_result,
        aux,
        aux_1,
        aux_2,
        aux_3,
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
        aux_prim_6,
        aux_prim_7,
        aux_prim_8,
        okr_1,
        name="IsRowAllOnes_U",
    )
    qc.append(
        UAny().to_gate(),
        [
            *i,
            *hasZero_1,
            *aux_prim,
            *aux_prim_1,
            *aux_prim_2,
            *aux_prim_3,
            *aux_prim_4,
            *aux_prim_5,
            *aux_prim_6,
            *aux_prim_7,
            *aux_prim_8,
        ],
    )
    qc.append(qiskit.circuit.Gate("BasicGateU SWAP", 2, []), [*hasZero, *hasZero_1])
    qc.append(
        qiskit.circuit.Gate(
            "RevEmbedU [hasZero] (UnOpE {un_op = NotOp, operand = VarE {var = hasZero}})",
            2,
            [],
        ),
        [*hasZero, *okr_1],
    )
    qc.append(qiskit.circuit.Gate("BasicGateU SWAP", 2, []), [*okr, *okr_1])
    return qc


# Grover[...]
def Grover(k):
    i = qiskit.circuit.QuantumRegister(5, "i")
    x = qiskit.circuit.QuantumRegister(4, "x")
    ret_1 = qiskit.circuit.QuantumRegister(1, "ret_1")
    aux_4 = qiskit.circuit.QuantumRegister(1, "aux_4")
    aux_5 = qiskit.circuit.QuantumRegister(1, "aux_5")
    aux_6 = qiskit.circuit.QuantumRegister(1, "aux_6")
    qc = qiskit.circuit.QuantumCircuit(i, x, ret_1, aux_4, aux_5, aux_6, name="Grover")
    qc.append(qiskit.circuit.Gate("BasicGateU XGate", 1, []), [*ret_1])
    qc.append(qiskit.circuit.Gate("BasicGateU HGate", 1, []), [*ret_1])
    qc.append(
        qiskit.circuit.Gate("DistrU (UniformE {sample_ty = Fin 10})", 4, []), [*x]
    )
    qc.append(qiskit.circuit.Gate("URepeatS", qc.num_qubits, []), qc.qubits)
    qc.append(qiskit.circuit.Gate("BasicGateU HGate", 1, []), [*ret_1])
    qc.append(qiskit.circuit.Gate("BasicGateU XGate", 1, []), [*ret_1])
    return qc


def QAny():
    i = qiskit.circuit.ClassicalRegister(5, "i")
    ret_1 = qiskit.circuit.ClassicalRegister(1, "ret_1")
    s_result_1 = qiskit.circuit.ClassicalRegister(4, "s_result_1")
    not_done = qiskit.circuit.ClassicalRegister(1, "not_done")
    Q_sum = qiskit.circuit.ClassicalRegister(5, "Q_sum")
    j = qiskit.circuit.ClassicalRegister(5, "j")
    j_lim = qiskit.circuit.ClassicalRegister(5, "j_lim")
    qc = qiskit.circuit.QuantumCircuit(
        i, ret_1, s_result_1, not_done, Q_sum, j, j_lim, name="QAny"
    )
    for _ in range(27):
        Q_sum = 0
        for j_lim in [1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3]:
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


def IsRowAllOnes():
    i = qiskit.circuit.ClassicalRegister(5, "i")
    okr = qiskit.circuit.ClassicalRegister(1, "okr")
    hasZero = qiskit.circuit.ClassicalRegister(1, "hasZero")
    qc = qiskit.circuit.QuantumCircuit(i, okr, hasZero, name="IsRowAllOnes")
    i, hasZero = QAny(i, hasZero)
    okr = not (hasZero)
    return qc


# UAny[Fin 20, 5.0e-4]
def UAny_1():
    ret_2 = qiskit.circuit.QuantumRegister(1, "ret_2")
    s_result_2 = qiskit.circuit.QuantumRegister(5, "s_result_2")
    aux_7 = qiskit.circuit.QuantumRegister(1, "aux_7")
    aux_8 = qiskit.circuit.QuantumRegister(1, "aux_8")
    aux_9 = qiskit.circuit.QuantumRegister(4, "aux_9")
    aux_10 = qiskit.circuit.QuantumRegister(1, "aux_10")
    aux_11 = qiskit.circuit.QuantumRegister(1, "aux_11")
    aux_12 = qiskit.circuit.QuantumRegister(1, "aux_12")
    aux_13 = qiskit.circuit.QuantumRegister(1, "aux_13")
    aux_14 = qiskit.circuit.QuantumRegister(59, "aux_14")
    aux_15 = qiskit.circuit.QuantumRegister(59, "aux_15")
    aux_16 = qiskit.circuit.QuantumRegister(118, "aux_16")
    aux_17 = qiskit.circuit.QuantumRegister(236, "aux_17")
    aux_18 = qiskit.circuit.QuantumRegister(4, "aux_18")
    aux_19 = qiskit.circuit.QuantumRegister(1, "aux_19")
    aux_20 = qiskit.circuit.QuantumRegister(1, "aux_20")
    aux_21 = qiskit.circuit.QuantumRegister(1, "aux_21")
    aux_22 = qiskit.circuit.QuantumRegister(1, "aux_22")
    aux_23 = qiskit.circuit.QuantumRegister(59, "aux_23")
    aux_24 = qiskit.circuit.QuantumRegister(59, "aux_24")
    aux_25 = qiskit.circuit.QuantumRegister(118, "aux_25")
    aux_26 = qiskit.circuit.QuantumRegister(236, "aux_26")
    aux_27 = qiskit.circuit.QuantumRegister(1, "aux_27")
    aux_28 = qiskit.circuit.QuantumRegister(1, "aux_28")
    ctrl_1 = qiskit.circuit.QuantumRegister(16, "ctrl_1")
    pred_out_1 = qiskit.circuit.QuantumRegister(16, "pred_out_1")
    n_iter_1 = qiskit.circuit.QuantumRegister(32, "n_iter_1")
    s_arg_1 = qiskit.circuit.QuantumRegister(80, "s_arg_1")
    qc = qiskit.circuit.QuantumCircuit(
        ret_2,
        s_result_2,
        aux_7,
        aux_8,
        aux_9,
        aux_10,
        aux_11,
        aux_12,
        aux_13,
        aux_14,
        aux_15,
        aux_16,
        aux_17,
        aux_18,
        aux_19,
        aux_20,
        aux_21,
        aux_22,
        aux_23,
        aux_24,
        aux_25,
        aux_26,
        aux_27,
        aux_28,
        ctrl_1,
        pred_out_1,
        n_iter_1,
        s_arg_1,
        name="UAny_1",
    )
    qc.append(qiskit.circuit.Gate("UForInRangeS", qc.num_qubits, []), qc.qubits)
    qc.append(
        qiskit.circuit.Gate(
            "RevEmbedU [a] (UnOpE {un_op = AnyOp, operand = VarE {var = a}})", 17, []
        ),
        [*pred_out_1, *ret_2],
    )
    qc.append(
        qiskit.circuit.Gate(
            "RevEmbedU [a,f] (BinOpE {bin_op = VecSelectOp, lhs = VarE {var = a}, rhs = VarE {var = f}})",
            101,
            [],
        ),
        [*s_arg_1, *pred_out_1, *s_result_2],
    )
    return qc


def HasAllOnesRow_U():
    ok = qiskit.circuit.QuantumRegister(1, "ok")
    ok_1 = qiskit.circuit.QuantumRegister(1, "ok_1")
    s_result_2 = qiskit.circuit.QuantumRegister(5, "s_result_2")
    aux_7 = qiskit.circuit.QuantumRegister(1, "aux_7")
    aux_8 = qiskit.circuit.QuantumRegister(1, "aux_8")
    aux_9 = qiskit.circuit.QuantumRegister(4, "aux_9")
    aux_10 = qiskit.circuit.QuantumRegister(1, "aux_10")
    aux_11 = qiskit.circuit.QuantumRegister(1, "aux_11")
    aux_12 = qiskit.circuit.QuantumRegister(1, "aux_12")
    aux_13 = qiskit.circuit.QuantumRegister(1, "aux_13")
    aux_14 = qiskit.circuit.QuantumRegister(59, "aux_14")
    aux_15 = qiskit.circuit.QuantumRegister(59, "aux_15")
    aux_16 = qiskit.circuit.QuantumRegister(118, "aux_16")
    aux_17 = qiskit.circuit.QuantumRegister(236, "aux_17")
    aux_18 = qiskit.circuit.QuantumRegister(4, "aux_18")
    aux_19 = qiskit.circuit.QuantumRegister(1, "aux_19")
    aux_20 = qiskit.circuit.QuantumRegister(1, "aux_20")
    aux_21 = qiskit.circuit.QuantumRegister(1, "aux_21")
    aux_22 = qiskit.circuit.QuantumRegister(1, "aux_22")
    aux_23 = qiskit.circuit.QuantumRegister(59, "aux_23")
    aux_24 = qiskit.circuit.QuantumRegister(59, "aux_24")
    aux_25 = qiskit.circuit.QuantumRegister(118, "aux_25")
    aux_26 = qiskit.circuit.QuantumRegister(236, "aux_26")
    aux_27 = qiskit.circuit.QuantumRegister(1, "aux_27")
    aux_28 = qiskit.circuit.QuantumRegister(1, "aux_28")
    ctrl_1 = qiskit.circuit.QuantumRegister(16, "ctrl_1")
    pred_out_1 = qiskit.circuit.QuantumRegister(16, "pred_out_1")
    n_iter_1 = qiskit.circuit.QuantumRegister(32, "n_iter_1")
    s_arg_1 = qiskit.circuit.QuantumRegister(80, "s_arg_1")
    aux_prim_9 = qiskit.circuit.QuantumRegister(5, "aux_prim_9")
    aux_prim_10 = qiskit.circuit.QuantumRegister(1, "aux_prim_10")
    aux_prim_11 = qiskit.circuit.QuantumRegister(1, "aux_prim_11")
    aux_prim_12 = qiskit.circuit.QuantumRegister(4, "aux_prim_12")
    aux_prim_13 = qiskit.circuit.QuantumRegister(1, "aux_prim_13")
    aux_prim_14 = qiskit.circuit.QuantumRegister(1, "aux_prim_14")
    aux_prim_15 = qiskit.circuit.QuantumRegister(1, "aux_prim_15")
    aux_prim_16 = qiskit.circuit.QuantumRegister(1, "aux_prim_16")
    aux_prim_17 = qiskit.circuit.QuantumRegister(59, "aux_prim_17")
    aux_prim_18 = qiskit.circuit.QuantumRegister(59, "aux_prim_18")
    aux_prim_19 = qiskit.circuit.QuantumRegister(118, "aux_prim_19")
    aux_prim_20 = qiskit.circuit.QuantumRegister(236, "aux_prim_20")
    aux_prim_21 = qiskit.circuit.QuantumRegister(4, "aux_prim_21")
    aux_prim_22 = qiskit.circuit.QuantumRegister(1, "aux_prim_22")
    aux_prim_23 = qiskit.circuit.QuantumRegister(1, "aux_prim_23")
    aux_prim_24 = qiskit.circuit.QuantumRegister(1, "aux_prim_24")
    aux_prim_25 = qiskit.circuit.QuantumRegister(1, "aux_prim_25")
    aux_prim_26 = qiskit.circuit.QuantumRegister(59, "aux_prim_26")
    aux_prim_27 = qiskit.circuit.QuantumRegister(59, "aux_prim_27")
    aux_prim_28 = qiskit.circuit.QuantumRegister(118, "aux_prim_28")
    aux_prim_29 = qiskit.circuit.QuantumRegister(236, "aux_prim_29")
    aux_prim_30 = qiskit.circuit.QuantumRegister(1, "aux_prim_30")
    aux_prim_31 = qiskit.circuit.QuantumRegister(1, "aux_prim_31")
    aux_prim_32 = qiskit.circuit.QuantumRegister(16, "aux_prim_32")
    aux_prim_33 = qiskit.circuit.QuantumRegister(16, "aux_prim_33")
    aux_prim_34 = qiskit.circuit.QuantumRegister(32, "aux_prim_34")
    aux_prim_35 = qiskit.circuit.QuantumRegister(80, "aux_prim_35")
    qc = qiskit.circuit.QuantumCircuit(
        ok,
        ok_1,
        s_result_2,
        aux_7,
        aux_8,
        aux_9,
        aux_10,
        aux_11,
        aux_12,
        aux_13,
        aux_14,
        aux_15,
        aux_16,
        aux_17,
        aux_18,
        aux_19,
        aux_20,
        aux_21,
        aux_22,
        aux_23,
        aux_24,
        aux_25,
        aux_26,
        aux_27,
        aux_28,
        ctrl_1,
        pred_out_1,
        n_iter_1,
        s_arg_1,
        aux_prim_9,
        aux_prim_10,
        aux_prim_11,
        aux_prim_12,
        aux_prim_13,
        aux_prim_14,
        aux_prim_15,
        aux_prim_16,
        aux_prim_17,
        aux_prim_18,
        aux_prim_19,
        aux_prim_20,
        aux_prim_21,
        aux_prim_22,
        aux_prim_23,
        aux_prim_24,
        aux_prim_25,
        aux_prim_26,
        aux_prim_27,
        aux_prim_28,
        aux_prim_29,
        aux_prim_30,
        aux_prim_31,
        aux_prim_32,
        aux_prim_33,
        aux_prim_34,
        aux_prim_35,
        name="HasAllOnesRow_U",
    )
    qc.append(
        UAny_1().to_gate(),
        [
            *ok_1,
            *aux_prim_9,
            *aux_prim_10,
            *aux_prim_11,
            *aux_prim_12,
            *aux_prim_13,
            *aux_prim_14,
            *aux_prim_15,
            *aux_prim_16,
            *aux_prim_17,
            *aux_prim_18,
            *aux_prim_19,
            *aux_prim_20,
            *aux_prim_21,
            *aux_prim_22,
            *aux_prim_23,
            *aux_prim_24,
            *aux_prim_25,
            *aux_prim_26,
            *aux_prim_27,
            *aux_prim_28,
            *aux_prim_29,
            *aux_prim_30,
            *aux_prim_31,
            *aux_prim_32,
            *aux_prim_33,
            *aux_prim_34,
            *aux_prim_35,
        ],
    )
    qc.append(qiskit.circuit.Gate("BasicGateU SWAP", 2, []), [*ok, *ok_1])
    return qc


# Grover[...]
def Grover_1(k):
    x_1 = qiskit.circuit.QuantumRegister(5, "x_1")
    ret_3 = qiskit.circuit.QuantumRegister(1, "ret_3")
    aux_29 = qiskit.circuit.QuantumRegister(1, "aux_29")
    aux_30 = qiskit.circuit.QuantumRegister(1, "aux_30")
    aux_31 = qiskit.circuit.QuantumRegister(4, "aux_31")
    aux_32 = qiskit.circuit.QuantumRegister(1, "aux_32")
    aux_33 = qiskit.circuit.QuantumRegister(1, "aux_33")
    aux_34 = qiskit.circuit.QuantumRegister(1, "aux_34")
    aux_35 = qiskit.circuit.QuantumRegister(1, "aux_35")
    aux_36 = qiskit.circuit.QuantumRegister(59, "aux_36")
    aux_37 = qiskit.circuit.QuantumRegister(59, "aux_37")
    aux_38 = qiskit.circuit.QuantumRegister(118, "aux_38")
    aux_39 = qiskit.circuit.QuantumRegister(236, "aux_39")
    aux_40 = qiskit.circuit.QuantumRegister(4, "aux_40")
    aux_41 = qiskit.circuit.QuantumRegister(1, "aux_41")
    aux_42 = qiskit.circuit.QuantumRegister(1, "aux_42")
    aux_43 = qiskit.circuit.QuantumRegister(1, "aux_43")
    aux_44 = qiskit.circuit.QuantumRegister(1, "aux_44")
    aux_45 = qiskit.circuit.QuantumRegister(59, "aux_45")
    aux_46 = qiskit.circuit.QuantumRegister(59, "aux_46")
    aux_47 = qiskit.circuit.QuantumRegister(118, "aux_47")
    aux_48 = qiskit.circuit.QuantumRegister(236, "aux_48")
    aux_49 = qiskit.circuit.QuantumRegister(1, "aux_49")
    qc = qiskit.circuit.QuantumCircuit(
        x_1,
        ret_3,
        aux_29,
        aux_30,
        aux_31,
        aux_32,
        aux_33,
        aux_34,
        aux_35,
        aux_36,
        aux_37,
        aux_38,
        aux_39,
        aux_40,
        aux_41,
        aux_42,
        aux_43,
        aux_44,
        aux_45,
        aux_46,
        aux_47,
        aux_48,
        aux_49,
        name="Grover_1",
    )
    qc.append(qiskit.circuit.Gate("BasicGateU XGate", 1, []), [*ret_3])
    qc.append(qiskit.circuit.Gate("BasicGateU HGate", 1, []), [*ret_3])
    qc.append(
        qiskit.circuit.Gate("DistrU (UniformE {sample_ty = Fin 20})", 5, []), [*x_1]
    )
    qc.append(qiskit.circuit.Gate("URepeatS", qc.num_qubits, []), qc.qubits)
    qc.append(qiskit.circuit.Gate("BasicGateU HGate", 1, []), [*ret_3])
    qc.append(qiskit.circuit.Gate("BasicGateU XGate", 1, []), [*ret_3])
    return qc


def QAny_1():
    ret_3 = qiskit.circuit.ClassicalRegister(1, "ret_3")
    s_result_3 = qiskit.circuit.ClassicalRegister(5, "s_result_3")
    not_done_1 = qiskit.circuit.ClassicalRegister(1, "not_done_1")
    Q_sum_1 = qiskit.circuit.ClassicalRegister(6, "Q_sum_1")
    j_1 = qiskit.circuit.ClassicalRegister(6, "j_1")
    j_lim_1 = qiskit.circuit.ClassicalRegister(6, "j_lim_1")
    qc = qiskit.circuit.QuantumCircuit(
        ret_3, s_result_3, not_done_1, Q_sum_1, j_1, j_lim_1, name="QAny_1"
    )
    for _ in range(7):
        Q_sum_1 = 0
        for j_lim_1 in [1, 1, 1, 2, 2, 2, 3, 4, 4, 4, 4, 4, 4, 4]:
            j_1 = random.randrange(j_lim_1)
            Q_sum_1 = Q_sum_1 + j_1
            not_done_1 = not_done_1 and (Q_sum_1 <= j_lim_1)
            if not_done_1:
                qc.append(
                    qiskit.circuit.Gate("UProcAndMeas", qc.num_qubits, []), qc.qubits
                )
                qc.append(
                    qiskit.circuit.Gate("UProcAndMeas", qc.num_qubits, []), qc.qubits
                )
                not_done_1 = not_done_1 and ret_3
            else:
                pass
    return qc


def HasAllOnesRow():
    ok = qiskit.circuit.ClassicalRegister(1, "ok")
    qc = qiskit.circuit.QuantumCircuit(ok, name="HasAllOnesRow")
    ok = QAny_1(ok)
    return qc
