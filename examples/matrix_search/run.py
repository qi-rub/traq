def main():
    from matrix_search import HasAllOnesRow_U
    bloq = HasAllOnesRow_U()
    _, counts = bloq.call_graph()
    print(counts)

def main_qiskit():
    import importlib
    mod = importlib.import_module("matrix_search-qiskit")

    # qc = mod.HasAllOnesRow_U()
    qc = mod.IsEntryZero()

    # print(qc.draw(output="text"))
    print(f"qubits: {qc.num_qubits}, cbits: {qc.num_clbits}")

    qc.draw(output="mpl", filename="matrix_search-qiskit.png")
    print("Saved to matrix_search-qiskit.png")

if __name__ == '__main__':
    main_qiskit()
