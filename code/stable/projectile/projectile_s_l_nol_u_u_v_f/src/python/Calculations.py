## \file Calculations.py
# \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
# \brief Provides functions for calculating the outputs
import math

## \brief Calculates flight duration: the time when the projectile lands (s)
# \param v_launch launch speed: the initial speed of the projectile when launched (m/s)
# \param theta launch angle: the angle between the launcher and a straight line from the launcher to the target (rad)
# \param g magnitude of gravitational acceleration (m/s^2)
# \return flight duration: the time when the projectile lands (s)
def func_t_flight(v_launch, theta, g):
    return 2.0 * v_launch * math.sin(theta) / g

## \brief Calculates landing position: the distance from the launcher to the final position of the projectile (m)
# \param v_launch launch speed: the initial speed of the projectile when launched (m/s)
# \param theta launch angle: the angle between the launcher and a straight line from the launcher to the target (rad)
# \param g magnitude of gravitational acceleration (m/s^2)
# \return landing position: the distance from the launcher to the final position of the projectile (m)
def func_p_land(v_launch, theta, g):
    return 2.0 * v_launch ** 2.0 * math.sin(theta) * math.cos(theta) / g

## \brief Calculates distance between the target position and the landing position: the offset between the target position and the landing position (m)
# \param p_target target position: the distance from the launcher to the target (m)
# \param p_land landing position: the distance from the launcher to the final position of the projectile (m)
# \return distance between the target position and the landing position: the offset between the target position and the landing position (m)
def func_d_offset(p_target, p_land):
    return p_land - p_target

## \brief Calculates output message as a string
# \param p_target target position: the distance from the launcher to the target (m)
# \param epsilon hit tolerance
# \param d_offset distance between the target position and the landing position: the offset between the target position and the landing position (m)
# \return output message as a string
def func_s(p_target, epsilon, d_offset):
    if (math.fabs(d_offset / p_target) < epsilon) :
        return "The target was hit."
    elif (d_offset < 0.0) :
        return "The projectile fell short."
    else :
        return "The projectile went long."
