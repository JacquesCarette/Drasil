from __future__ import print_function
import sys
import math

import InputFormat
import InputParameters
import OutputFormat

inputfile = sys.argv[1]
InputFormat.func_get_input(inputfile, A_C, C_W, h_C, T_init, t_final, L, T_C, t_step, rho_W, D, A_tol, R_tol, T_W, E_W)
InputParameters.input_constraints(A_C, C_W, h_C, T_init, t_final, L, T_C, t_step, rho_W, D, T_W, E_W)
OutputFormat.write_output(T_W, E_W)


