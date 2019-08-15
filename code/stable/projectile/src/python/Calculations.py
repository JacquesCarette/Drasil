## \file Calculations.py
# \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
# \brief Provides functions for calculating the outputs
from __future__ import print_function
import sys
import math

import InputParameters

## \brief Calculates flight duration: the time when the projectile lands (s)
# \param inParams structure holding the input values
# \return flight duration: the time when the projectile lands (s)
def func_t_flight(inParams):
    return 2 * inParams.v_launch * math.sin(inParams.theta) / 9.8

## \brief Calculates landing position: the distance from the launcher to the final position of the projectile (m)
# \param inParams structure holding the input values
# \return landing position: the distance from the launcher to the final position of the projectile (m)
def func_p_land(inParams):
    return 2 * inParams.v_launch ** 2 * math.sin(inParams.theta) * math.cos(inParams.theta) / 9.8

## \brief Calculates distance between the target position and the landing position: the offset between the target position and the landing position (m)
# \param inParams structure holding the input values
# \param p_land landing position: the distance from the launcher to the final position of the projectile (m)
# \return distance between the target position and the landing position: the offset between the target position and the landing position (m)
def func_d_offset(inParams, p_land):
    return p_land - inParams.p_target

## \brief Calculates output message as a string
# \param inParams structure holding the input values
# \param d_offset distance between the target position and the landing position: the offset between the target position and the landing position (m)
# \return output message as a string
def func_s(inParams, d_offset):
    if (math.fabs(d_offset / inParams.p_target) < 2.0e-2) :
        return "The target was hit."
    elif (d_offset < 0) :
        return "The projectile fell short."
    else :
        return "The projectile went long."


