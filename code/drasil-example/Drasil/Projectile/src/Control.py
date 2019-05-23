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
d = Calculations.func_d(inParams)
short = Calculations.func_short(inParams, d)
offset = Calculations.func_offset(inParams, d)
hit = Calculations.func_hit(inParams, offset)
OutputFormat.write_output(hit, short, offset)


