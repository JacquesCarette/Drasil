## \file Control.py
# \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
# \brief Controls the flow of the program
import sys

import Calculations
import InputConstraints
import InputFormat
import OutputFormat

filename = sys.argv[1]
g_vect = 9.8
epsilon = 2.0e-2
v_launch, theta, p_target = InputFormat.get_input(filename)
InputConstraints.input_constraints(v_launch, theta, p_target)
t_flight = Calculations.func_t_flight(v_launch, theta, g_vect)
p_land = Calculations.func_p_land(v_launch, theta, g_vect)
d_offset = Calculations.func_d_offset(p_target, p_land)
s = Calculations.func_s(p_target, epsilon, d_offset)
OutputFormat.write_output(s, d_offset)
