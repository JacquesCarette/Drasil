from __future__ import print_function
import sys
import math
import InputParameters
import InputConstraints
import InputFormat
import OutputFormat
import Calculations


inputfile = sys.argv[1]
inParams = InputParameters.InputParameters()
InputFormat.func_get_input(inputfile, inParams)
InputConstraints.input_constraints(inParams)
p_land = Calculations.func_p_land(inParams)
offset = Calculations.func_offset(inParams, p_land)
message = Calculations.func_message(inParams, offset)
OutputFormat.write_output(message, offset)


