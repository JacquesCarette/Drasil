from __future__ import print_function
import sys
import math
import InputParameters


def check_constraints(inparams):
    if (inparams.a <= 0.0) :
        raise Exception("InputError: a must be greater than 0")
    if (inparams.b <= 0.0) :
        raise Exception("InputError: b must be greater than 0")
    if (inparams.asprat < 1.0) :
        raise Exception("InputError: a/b cannot be less than 1.0")
    if (inparams.asprat > 5.0) :
        raise Exception("InputError: a/b cannot be greater than 5.0")
    if (not((((((((((((inparams.t == 2.5) or (inparams.t == 2.7)) or (inparams.t == 3.0)) or (inparams.t == 4.0)) or (inparams.t == 5.0)) or (inparams.t == 6.0)) or (inparams.t == 8.0)) or (inparams.t == 10.0)) or (inparams.t == 12.0)) or (inparams.t == 16.0)) or (inparams.t == 19.0)) or (inparams.t == 22.0))) :
        raise Exception("InputError: t must be in [2.5,2.7,3.0,4.0,5.0,6.0,8.0,10.0,12.0,16.0,19.0,22.0]")
    if (inparams.tnt <= 0.0) :
        raise Exception("InputError: tnt must be greater than 0")
    if (inparams.wtnt < 4.5) :
        raise Exception("InputError: wtnt cannot be less than 4.5")
    if (inparams.wtnt > 910.0) :
        raise Exception("InputError: wtnt cannot be greater than 910.0")
    if (inparams.sd < 6.0) :
        raise Exception("InputError: sd cannot be less than 6.0")
    if (inparams.sd > 130.0) :
        raise Exception("InputError: sd cannot be greater than 130.0")


