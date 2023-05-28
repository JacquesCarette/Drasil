/** \file Calculations.hpp
    \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
    \brief Provides functions for calculating the outputs
*/
#ifndef Calculations_h
#define Calculations_h

#include <string>

#include "InputParameters.hpp"

using std::string;

/** \brief Calculates flight duration: the time when the projectile lands (s)
    \param inParams structure holding the input values
    \param g magnitude of gravitational acceleration (m/s^2)
    \return flight duration: the time when the projectile lands (s)
*/
double func_t_flight(InputParameters &inParams, double g);

/** \brief Calculates landing position: the distance from the launcher to the final position of the projectile (m)
    \param inParams structure holding the input values
    \param g magnitude of gravitational acceleration (m/s^2)
    \return landing position: the distance from the launcher to the final position of the projectile (m)
*/
double func_p_land(InputParameters &inParams, double g);

/** \brief Calculates distance between the target position and the landing position: the offset between the target position and the landing position (m)
    \param inParams structure holding the input values
    \param p_land landing position: the distance from the launcher to the final position of the projectile (m)
    \return distance between the target position and the landing position: the offset between the target position and the landing position (m)
*/
double func_d_offset(InputParameters &inParams, double p_land);

/** \brief Calculates output message as a string
    \param inParams structure holding the input values
    \param epsilon hit tolerance
    \param d_offset distance between the target position and the landing position: the offset between the target position and the landing position (m)
    \return output message as a string
*/
string func_s(InputParameters &inParams, double epsilon, double d_offset);

#endif
