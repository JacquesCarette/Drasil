/** \file InputConstraints.hpp
    \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
    \brief Provides the function for checking the physical constraints on the input
*/
#ifndef InputConstraints_h
#define InputConstraints_h

#define _USE_MATH_DEFINES

#include <math.h>

/** \brief Verifies that input values satisfy the physical constraints
    \param v_launch launch speed: the initial speed of the projectile when launched (m/s)
    \param theta launch angle: the angle between the launcher and a straight line from the launcher to the target (rad)
    \param p_target target position: the distance from the launcher to the target (m)
*/
void input_constraints(float v_launch, float theta, float p_target);

#endif
