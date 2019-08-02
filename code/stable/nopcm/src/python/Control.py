## \file Control.py
# \brief Controls the flow of the program
from __future__ import print_function
import sys
import math

import InputParameters
import OutputFormat

filename = sys.argv[1]
A_C, C_W, h_C, T_init, t_final, L, T_C, t_step, rho_W, D, A_tol, R_tol, T_W, E_W = InputParameters.get_input(filename)
InputParameters.input_constraints(A_C, C_W, h_C, T_init, t_final, L, T_C, t_step, rho_W, D, T_W, E_W)
OutputFormat.write_output(T_W, E_W)


