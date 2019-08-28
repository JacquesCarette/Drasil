## \file Control.py
# \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
# \brief Controls the flow of the program
from __future__ import print_function
import sys
import math

import InputParameters
import InputFormat
import InputConstraints
import OutputFormat
import Calculations

filename = sys.argv[1]
inParams = InputParameters.InputParameters()
g = 9.8
pi = 3.14159265
epsilon = 2.0e-2
InputFormat.get_input(filename, inParams)
InputConstraints.input_constraints(inParams, pi)
t_flight = Calculations.func_t_flight(inParams, g)
p_land = Calculations.func_p_land(inParams, g)
d_offset = Calculations.func_d_offset(inParams, p_land)
s = Calculations.func_s(inParams, epsilon, d_offset)
OutputFormat.write_output(s, d_offset)


