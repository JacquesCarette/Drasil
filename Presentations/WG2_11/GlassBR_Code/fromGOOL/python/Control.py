from __future__ import print_function
import sys
import math
import InputParameters
import InputFormat
import DerivedValues
import InputConstraints
import Interpolation
import Calculations
import OutputFormat
import ReadTable


filename = sys.argv[1]
inparams = InputParameters.InputParameters()
InputFormat.get_input(filename, inparams)
DerivedValues.derived_params(inparams)
InputConstraints.check_constraints(inparams)
w_array = ReadTable.read_z_array("TSD.txt")
data_sd = ReadTable.read_x_array("TSD.txt")
data_q = ReadTable.read_y_array("TSD.txt")
j_array = ReadTable.read_z_array("SDF.txt")
data_asprat = ReadTable.read_x_array("SDF.txt")
data_qstar = ReadTable.read_y_array("SDF.txt")
q = Interpolation.interpY(data_sd, data_q, w_array, inparams.sd, inparams.wtnt)
q_hat = Calculations.calc_q_hat(q, inparams)
j_tol = Calculations.calc_j_tol(inparams)
j = Interpolation.interpZ(data_asprat, data_qstar, j_array, inparams.asprat, q_hat)
q_hat_tol = Interpolation.interpY(data_asprat, data_qstar, j_array, inparams.asprat, j_tol)
pb = Calculations.calc_pb(j, inparams)
nfl = Calculations.calc_nfl(q_hat_tol, inparams)
lr = Calculations.calc_lr(nfl, inparams)
is_safe1 = Calculations.calc_is_safe1(pb, inparams)
is_safe2 = Calculations.calc_is_safe2(lr, q)
OutputFormat.display_output("outputfile.txt", q, j, q_hat_tol, pb, lr, nfl, is_safe1, is_safe2, inparams)
print("Main has been executed and the results have been written to 'outputfile.txt'.");


