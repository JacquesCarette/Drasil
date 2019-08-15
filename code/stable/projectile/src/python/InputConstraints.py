## \file InputConstraints.py
# \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
# \brief Provides the function for checking the physical constraints and software constraints on the input
from __future__ import print_function
import sys
import math

import InputParameters

## \brief Verifies that input values satisfy the physical constraints and software constraints
# \param inParams structure holding the input values
def input_constraints(inParams):
    if (not(inParams.v_launch > 0)) :
        print("Warning: ", end='')
        print("v_launch has value ", end='')
        print(inParams.v_launch, end='')
        print(" but suggested to be ", end='')
        print("above ", end='')
        print(0, end='')
        print(".")
    if (not(0 < inParams.theta and inParams.theta < 3.14159265 / 2)) :
        print("Warning: ", end='')
        print("theta has value ", end='')
        print(inParams.theta, end='')
        print(" but suggested to be ", end='')
        print("between ", end='')
        print(0, end='')
        print(" and ", end='')
        print(3.14159265 / 2, end='')
        print(" ((pi)/(2))", end='')
        print(".")
    if (not(inParams.p_target > 0)) :
        print("Warning: ", end='')
        print("p_target has value ", end='')
        print(inParams.p_target, end='')
        print(" but suggested to be ", end='')
        print("above ", end='')
        print(0, end='')
        print(".")


