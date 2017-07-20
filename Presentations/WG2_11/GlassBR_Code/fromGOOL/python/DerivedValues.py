from __future__ import print_function
import sys
import math
import InputParameters


def derived_params(inparams):
    inparams.asprat = inparams.a / inparams.b
    inparams.sd = math.sqrt(((inparams.sdx ** 2.0) + (inparams.sdy ** 2.0)) + (inparams.sdz ** 2.0))
    inparams.ldf = (inparams.td / 60.0) ** (inparams.m / 16.0)
    inparams.wtnt = inparams.w * inparams.tnt
    if (inparams.t == 2.5) :
        inparams.h = 2.16
    elif (inparams.t == 2.7) :
        inparams.h = 2.59
    elif (inparams.t == 3.0) :
        inparams.h = 2.92
    elif (inparams.t == 4.0) :
        inparams.h = 3.78
    elif (inparams.t == 5.0) :
        inparams.h = 4.57
    elif (inparams.t == 6.0) :
        inparams.h = 5.56
    elif (inparams.t == 8.0) :
        inparams.h = 7.42
    elif (inparams.t == 10.0) :
        inparams.h = 9.02
    elif (inparams.t == 12.0) :
        inparams.h = 11.91
    elif (inparams.t == 16.0) :
        inparams.h = 15.09
    elif (inparams.t == 19.0) :
        inparams.h = 18.26
    elif (inparams.t == 22.0) :
        inparams.h = 21.44
    if (inparams.gt == 1) :
        inparams.gtf = 1.0
    elif (inparams.gt == 2) :
        inparams.gtf = 2.0
    elif (inparams.gt == 3) :
        inparams.gtf = 4.0


