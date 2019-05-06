from __future__ import print_function
import sys
import math
import InputParameters


def input_constraints(inParams):
    if (not((0.1 <= inParams.a) and (inParams.a <= 5.0))) :
        raise Exception("InputError")
    if (not((0.1 <= inParams.b) and (inParams.b <= 5.0))) :
        raise Exception("InputError")
    if (not((6.0 <= inParams.SD) and (inParams.SD <= 130.0))) :
        raise Exception("InputError")
    if (not((4.5 <= inParams.w) and (inParams.w <= 910.0))) :
        raise Exception("InputError")
    if (not(inParams.AR <= 5.0)) :
        raise Exception("InputError")
    if (not(inParams.a > 0)) :
        raise Exception("InputError")
    if (not(inParams.a >= inParams.b)) :
        raise Exception("InputError")
    if (not((0 < inParams.b) and (inParams.b <= inParams.a))) :
        raise Exception("InputError")
    if (not(inParams.SD > 0)) :
        raise Exception("InputError")
    if (not(inParams.w > 0)) :
        raise Exception("InputError")
    if (not(inParams.AR >= 1)) :
        raise Exception("InputError")
    if (not((0 < inParams.P_btol) and (inParams.P_btol < 1))) :
        raise Exception("InputError")
    if (not(inParams.TNT > 0)) :
        raise Exception("InputError")


