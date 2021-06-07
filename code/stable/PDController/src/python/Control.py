## \file Control.py
# \author Naveen Ganesh Muralidharan
# \brief Controls the flow of the program
import sys

import Calculations
import InputParameters
import OutputFormat

filename = sys.argv[1]
r_t, K_d, K_p, t_step, t_sim = InputParameters.get_input(filename)
InputParameters.input_constraints(r_t, K_d, K_p, t_step, t_sim)
y_t = Calculations.func_y_t(K_d, K_p, r_t, t_sim, t_step)
OutputFormat.write_output(y_t)
