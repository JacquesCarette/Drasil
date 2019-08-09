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
InputFormat.get_input(filename, inParams)
InputConstraints.input_constraints(inParams)
t_flight = Calculations.func_t_flight(inParams)
p_land = Calculations.func_p_land(inParams)
d_offset = Calculations.func_d_offset(inParams, p_land)
s = Calculations.func_s(inParams, d_offset)
OutputFormat.write_output(s, d_offset)


