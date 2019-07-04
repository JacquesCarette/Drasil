from __future__ import print_function
import sys
import math

import InputParameters

def func_t_flight(inParams):
    return ((2 * (inParams.v_launch * math.sin(inParams.angle))) / 9.8)

def func_p_land(inParams):
    return ((2 * ((inParams.v_launch ** 2) * (math.sin(inParams.angle) * math.cos(inParams.angle)))) / 9.8)

def func_d_offset(inParams, p_land):
    return (p_land - inParams.p_target)

def func_s(inParams, d_offset):
    if ((math.fabs((d_offset / inParams.p_target)) < 2.0e-2)) :
        return "The target was hit."
    elif ((d_offset < 0)) :
        return "The projectile fell short."
    elif ((d_offset > 0)) :
        return "The projectile went long."


