/** \file Calculations.hpp
    \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
    \brief Provides functions for calculating the outputs
*/
#ifndef Calculations_h
#define Calculations_h

#include <string>

using std::string;

/** \brief Calculates flight duration: the time when the projectile lands (s)
    \param v_launch launch speed: the initial speed of the projectile when launched (m/s)
    \param theta launch angle: the angle between the launcher and a straight line from the launcher to the target (rad)
    \param g magnitude of gravitational acceleration (m/s^2)
    \return flight duration: the time when the projectile lands (s)
*/
float func_t_flight(float v_launch, float theta, float g);

/** \brief Calculates landing position: the distance from the launcher to the final position of the projectile (m)
    \param v_launch launch speed: the initial speed of the projectile when launched (m/s)
    \param theta launch angle: the angle between the launcher and a straight line from the launcher to the target (rad)
    \param g magnitude of gravitational acceleration (m/s^2)
    \return landing position: the distance from the launcher to the final position of the projectile (m)
*/
float func_p_land(float v_launch, float theta, float g);

/** \brief Calculates distance between the target position and the landing position: the offset between the target position and the landing position (m)
    \param p_target target position: the distance from the launcher to the target (m)
    \param p_land landing position: the distance from the launcher to the final position of the projectile (m)
    \return distance between the target position and the landing position: the offset between the target position and the landing position (m)
*/
float func_d_offset(float p_target, float p_land);

/** \brief Calculates output message as a string
    \param p_target target position: the distance from the launcher to the target (m)
    \param epsilon hit tolerance
    \param d_offset distance between the target position and the landing position: the offset between the target position and the landing position (m)
    \return output message as a string
*/
string func_s(float p_target, float epsilon, float d_offset);

#endif
