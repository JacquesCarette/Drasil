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
    w_array, data_sd, data_q = readTable.read_table('TSD.txt')
    j_array, data_asprat, data_qstar = readTable.read_table('SDF.txt')

    q = calculations.calc_q(w_array, data_sd, data_q, params)
    j, q_hat_tol = calculations.calc_j(j_array, data_asprat, data_qstar, q, params)
    pb = calculations.calc_pb(j, params)
    lr, nfl = calculations.calc_lr(q_hat_tol, params)
    is_safe1, is_safe2, safe = calculations.is_safe(pb, lr, q, params)

    outputFormat.display_output('outputfile.txt', q, j, q_hat_tol, pb, lr, nfl, is_safe1, is_safe2, safe, params)
    print("Main has been executed and the results have been written to 'outputfile.txt'.")

if __name__ == "__main__":
    filename = sys.argv[1]
    main(filename)
