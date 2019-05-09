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
InputFormat.func_get_input(inputfile, inParams)
DerivedValues.derived_values(inParams)
InputConstraints.input_constraints(inParams)
q = Calculations.func_q(inParams)
J_tol = Calculations.func_J_tol(inParams)
q = Calculations.func_q(inParams)
q_hat = Calculations.func_q_hat(inParams, q)
q_hat_tol = Calculations.func_q_hat_tol(inParams, J_tol)
J = Calculations.func_J(inParams, q_hat)
NFL = Calculations.func_NFL(inParams, q_hat_tol)
B = Calculations.func_B(inParams, J)
LR = Calculations.func_LR(inParams, NFL)
is_safeLR = Calculations.func_is_safeLR(LR, q)
P_b = Calculations.func_P_b(B)
is_safePb = Calculations.func_is_safePb(inParams, P_b)
OutputFormat.write_output(is_safePb, is_safeLR, P_b)


