from __future__ import print_function
import sys
import math
import InputParameters


def func_d(inParams):
    return (2 * (inParams.v ** 2) * math.sin(math.radians(inParams.theta)) * math.cos(math.radians(inParams.theta))) / 9.8

def func_offset(inParams, d):
    return abs(inParams.d - d)

def func_hit(inParams, offset):
    return (offset < 0.02 * inParams.d)


