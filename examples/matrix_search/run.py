def main():
    from matrix_search import HasAllOnesRow_U
    bloq = HasAllOnesRow_U()
    _, counts = bloq.call_graph()
    print(counts)

if __name__ == '__main__':
    main()
