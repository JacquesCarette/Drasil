"""
Control Module
Secret: The algorithm for coordinating the running of the program.
Service: Provides the main program.
"""

import sys
from . import inputFormat
from . import readTable
from . import outputFormat
from . import param
from . import calculations
from . import derivedValues
from . import checkConstraints


def main(filename):
    params = param.Param()
    inputFormat.get_input(filename, params)
    derivedValues.derived_params(params)
    checkConstraints.check_constraints(params)
    
    w_array = readTable.read_num_col('TSD.txt')
    data_sd = readTable.read_array1('TSD.txt', len(w_array))
    data_q = readTable.read_array2('TSD.txt', len(w_array))
    
    j_array = readTable.read_num_col('SDF.txt')
    data_asprat = readTable.read_array1('SDF.txt', len(j_array))
    data_qstar = readTable.read_array2('SDF.txt', len(j_array))

    q = calculations.calc_q(w_array, data_sd, data_q, params)
    q_vec = calculations.calc_q_vec(j_array, data_asprat, data_qstar, params)
    j_vec = calculations.calc_j_vec(j_array, data_asprat, data_qstar, params)
    q_hat = calculations.calc_q_hat(q, q_vec, params)
    j_tol = calculations.calc_j_tol(params)
    j = calculations.calc_j(q_vec, j_vec, q_hat)
    q_hat_tol = calculations.calc_q_hat_tol(q_vec, j_vec, j_tol)
    pb = calculations.calc_pb(j, params)
    nfl = calculations.calc_nfl(q_hat_tol, params)
    lr = calculations.calc_lr(nfl, params)
    is_safe1 = calculations.is_safe1(pb, params)
    is_safe2 = calculations.is_safe2(lr, q)
    outputFormat.display_output('outputfile.txt', q, j, q_hat_tol, pb, lr, nfl, is_safe1, is_safe2, params)
    print("Main has been executed and the results have been written to 'outputfile.txt'.")

if __name__ == "__main__":
    filename = sys.argv[1]
    main(filename)
