from __future__ import print_function
import sys
import math

import InputFormat
import InputParameters
import OutputFormat

inputfile = sys.argv[1]
A_C = 0.0
C_W = 0.0
h_C = 0.0
T_init = 0.0
t_final = 0.0
L = 0.0
T_C = 0.0
t_step = 0.0
rho_W = 0.0
D = 0.0
A_tol = 0.0
R_tol = 0.0
T_W = 0.0
E_W = 0.0
InputFormat.func_get_input(inputfile, A_C, C_W, h_C, T_init, t_final, L, T_C, t_step, rho_W, D, A_tol, R_tol, T_W, E_W)
InputParameters.input_constraints(A_C, C_W, h_C, T_init, t_final, L, T_C, t_step, rho_W, D, T_W, E_W)
OutputFormat.write_output(T_W, E_W)


