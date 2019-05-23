from __future__ import print_function
import sys
import math
import InputParameters


def input_constraints(inParams):
    if (not(0 < inParams.v)):
        raise Exception("InputError")
    if (not((0 < inParams.theta) and (inParams.theta < 90))):
        raise Exception("InputError")
    if (not(0 < inParams.d)):
        raise Exception("InputError")


