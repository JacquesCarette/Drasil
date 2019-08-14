## \file InputConstraints.py
# \author Nikitha Krithnan and W. Spencer Smith
# \brief Provides the function for checking the physical constraints and software constraints on the input
from __future__ import print_function
import sys
import math

import InputParameters

## \brief Verifies that input values satisfy the physical constraints and software constraints
# \param inParams structure holding the input values
def input_constraints(inParams):
    outfile = open("log.txt", "a")
    print("function input_constraints called with inputs: {", file=outfile)
    print("  inParams = ", end='', file=outfile)
    print("Instance of InputParameters object", file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    if (not(0.1 <= inParams.a and inParams.a <= 5.0)) :
        print("a has value ", end='')
        print(inParams.a, end='')
        print(" but expected to be ", end='')
        print("between ", end='')
        print(0.1, end='')
        print(" (d_min)", end='')
        print(" and ", end='')
        print(5.0, end='')
        print(" (d_max)", end='')
        print(".")
        raise Exception("InputError")
    if (not(0.1 <= inParams.b and inParams.b <= 5.0)) :
        print("b has value ", end='')
        print(inParams.b, end='')
        print(" but expected to be ", end='')
        print("between ", end='')
        print(0.1, end='')
        print(" (d_min)", end='')
        print(" and ", end='')
        print(5.0, end='')
        print(" (d_max)", end='')
        print(".")
        raise Exception("InputError")
    if (not(4.5 <= inParams.w and inParams.w <= 910.0)) :
        print("w has value ", end='')
        print(inParams.w, end='')
        print(" but expected to be ", end='')
        print("between ", end='')
        print(4.5, end='')
        print(" (w_min)", end='')
        print(" and ", end='')
        print(910.0, end='')
        print(" (w_max)", end='')
        print(".")
        raise Exception("InputError")
    if (not(6.0 <= inParams.SD and inParams.SD <= 130.0)) :
        print("SD has value ", end='')
        print(inParams.SD, end='')
        print(" but expected to be ", end='')
        print("between ", end='')
        print(6.0, end='')
        print(" (SD_min)", end='')
        print(" and ", end='')
        print(130.0, end='')
        print(" (SD_max)", end='')
        print(".")
        raise Exception("InputError")
    if (not(inParams.AR <= 5.0)) :
        print("AR has value ", end='')
        print(inParams.AR, end='')
        print(" but expected to be ", end='')
        print("below ", end='')
        print(5.0, end='')
        print(" (AR_max)", end='')
        print(".")
        raise Exception("InputError")
    
    if (not(inParams.a > 0)) :
        print("a has value ", end='')
        print(inParams.a, end='')
        print(" but expected to be ", end='')
        print("above ", end='')
        print(0, end='')
        print(".")
        raise Exception("InputError")
    if (not(inParams.a >= inParams.b)) :
        print("a has value ", end='')
        print(inParams.a, end='')
        print(" but expected to be ", end='')
        print("above ", end='')
        print(inParams.b, end='')
        print(" (b)", end='')
        print(".")
        raise Exception("InputError")
    if (not(0 < inParams.b and inParams.b <= inParams.a)) :
        print("b has value ", end='')
        print(inParams.b, end='')
        print(" but expected to be ", end='')
        print("between ", end='')
        print(0, end='')
        print(" and ", end='')
        print(inParams.a, end='')
        print(" (a)", end='')
        print(".")
        raise Exception("InputError")
    if (not(inParams.w > 0)) :
        print("w has value ", end='')
        print(inParams.w, end='')
        print(" but expected to be ", end='')
        print("above ", end='')
        print(0, end='')
        print(".")
        raise Exception("InputError")
    if (not(0 <= inParams.P_btol and inParams.P_btol <= 1)) :
        print("P_btol has value ", end='')
        print(inParams.P_btol, end='')
        print(" but expected to be ", end='')
        print("between ", end='')
        print(0, end='')
        print(" and ", end='')
        print(1, end='')
        print(".")
        raise Exception("InputError")
    if (not(inParams.TNT > 0)) :
        print("TNT has value ", end='')
        print(inParams.TNT, end='')
        print(" but expected to be ", end='')
        print("above ", end='')
        print(0, end='')
        print(".")
        raise Exception("InputError")
    if (not(inParams.SD > 0)) :
        print("SD has value ", end='')
        print(inParams.SD, end='')
        print(" but expected to be ", end='')
        print("above ", end='')
        print(0, end='')
        print(".")
        raise Exception("InputError")
    if (not(inParams.AR >= 1)) :
        print("AR has value ", end='')
        print(inParams.AR, end='')
        print(" but expected to be ", end='')
        print("above ", end='')
        print(1, end='')
        print(".")
        raise Exception("InputError")


