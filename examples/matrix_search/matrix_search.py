import random
import qualtran as qlt


raise Exception('TODO UProcDecl')



def Matrix(arg_1 : int, arg_2 : int, arg_3 : int):
    raise Exception('external function - implement here')



raise Exception('TODO UProcBody')



def IsEntryZero(i0 : int, j0 : int, e_ : int):
    i0, j0, e = Matrix(i0, j0, e)
    e_ = not (e)
    return i0, j0, e_


# UAny[Fin 10, 2.6774112591424054e-13]
raise Exception('TODO UProcBody')



raise Exception('TODO UProcBody')


# Grover[...]
raise Exception('TODO UProcBody')



def QAny(i : int, ret_1 : int):
    for _ in range(27):
        Q_sum = 0
        for j_lim in [1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3]:
            j = random.randrange(j_lim)
            Q_sum = (Q_sum + j)
            not_done = (not_done and (Q_sum <= j_lim))
            if (not_done):
                raise Exception('TODO uproc-call-and-meas')
                raise Exception('TODO uproc-call-and-meas')
                not_done = (not_done and ret_1)
            else:
                pass
    return i, ret_1



def IsRowAllOnes(i : int, okr : int):
    i, hasZero = QAny(i, hasZero)
    okr = not (hasZero)
    return i, okr


# UAny[Fin 20, 5.0e-4]
raise Exception('TODO UProcBody')



raise Exception('TODO UProcBody')


# Grover[...]
raise Exception('TODO UProcBody')



def QAny_1(ret_3 : int):
    for _ in range(7):
        Q_sum_1 = 0
        for j_lim_1 in [1, 1, 1, 2, 2, 2, 3, 4, 4, 4, 4, 4, 4, 4]:
            j_1 = random.randrange(j_lim_1)
            Q_sum_1 = (Q_sum_1 + j_1)
            not_done_1 = (not_done_1 and (Q_sum_1 <= j_lim_1))
            if (not_done_1):
                raise Exception('TODO uproc-call-and-meas')
                raise Exception('TODO uproc-call-and-meas')
                not_done_1 = (not_done_1 and ret_3)
            else:
                pass
    return ret_3



def HasAllOnesRow(ok : int):
    ok = QAny_1(ok)
    return ok

