from __future__ import print_function
import sys
import math
import InputParameters


def func_p_land(inParams):
    return (2 * (inParams.v ** 2) * math.sin(inParams.theta) * math.cos(inParams.theta)) / 9.8

def func_offset(inParams, p_land):
    return p_land - inParams.p_target

def func_message(inParams, offset):
    if abs(offset / inParams.p_target) < 0.02:
        return "The target was hit."
    elif offset < 0:
        return "The projectile fell short."
    else:
        return "The projectile went long."

