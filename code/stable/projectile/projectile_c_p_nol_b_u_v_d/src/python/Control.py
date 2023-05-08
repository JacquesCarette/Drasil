## \file Control.py
# \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
# \brief Controls the flow of the program
import sys

import Calculations
import InputParameters
import OutputFormat

filename = sys.argv[1]
g_vect = 9.8
epsilon = 2.0e-2
inParams = InputParameters.InputParameters(filename)
t_flight = Calculations.func_t_flight(inParams, g_vect)
p_land = Calculations.func_p_land(inParams, g_vect)
d_offset = Calculations.func_d_offset(inParams, p_land)
s = Calculations.func_s(inParams, epsilon, d_offset)
OutputFormat.write_output(s, d_offset, t_flight)
