from __future__ import print_function
import sys
import math

import InputParameters

def input_constraints(inParams):
    if (not((inParams.v_launch > 0))) :
        print("Warning: constraint violated")
    if (not(((0 < inParams.theta) and (inParams.theta < (3.14159265 / 2))))) :
        print("Warning: constraint violated")
    if (not((inParams.p_target > 0))) :
        print("Warning: constraint violated")


