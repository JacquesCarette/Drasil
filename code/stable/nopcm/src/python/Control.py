from __future__ import print_function
import sys
import math
import InputParameters
import DerivedValues
import InputConstraints
import InputFormat
import OutputFormat
import Calculations


inputfile = sys.argv[1]
inParams = InputParameters.InputParameters()
func_get_input(inputfile, inParams)
DerivedValues.derived_values(inParams)
InputConstraints.input_constraints(inParams)
OutputFormat.write_output(inParams)


