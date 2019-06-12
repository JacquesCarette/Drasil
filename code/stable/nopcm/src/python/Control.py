from __future__ import print_function
import sys
import math

import InputConstraints
import InputParameters
import InputFormat
import Calculations
import OutputFormat

inputfile = sys.argv[1]
inParams = InputParameters.InputParameters()
InputFormat.func_get_input(inputfile, inParams)
InputConstraints.input_constraints(inParams)
OutputFormat.write_output(inParams)


