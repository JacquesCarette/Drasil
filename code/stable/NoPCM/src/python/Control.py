## \file Control.py
# \author Thulasi Jegatheesan
# \brief Controls the flow of the program
import sys

import Calculations
import InputParameters
import OutputFormat

filename = sys.argv[1]
A_C, C_W, h_C, T_init, t_final, L, T_C, t_step, rho_W, D, A_tol, R_tol, E_W = InputParameters.get_input(filename)
V_tank = InputParameters.derived_values(D, L)
InputParameters.input_constraints(A_C, C_W, h_C, T_init, t_final, L, T_C, t_step, rho_W, D, E_W)
V_W = Calculations.func_V_W(V_tank)
m_W = Calculations.func_m_W(rho_W, V_W)
tau_W = Calculations.func_tau_W(C_W, h_C, A_C, m_W)
T_W = Calculations.func_T_W(T_C, t_final, T_init, A_tol, R_tol, t_step, tau_W)
OutputFormat.write_output(E_W, T_W)
