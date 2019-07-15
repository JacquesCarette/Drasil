/** \file Calculations.hpp
*/
#ifndef Calculations_h
#define Calculations_h

#include <string>
#include <vector>

using std::string;
using std::vector;
using std::ifstream;
using std::ofstream;

#include "InputParameters.hpp"

/** \brief Calculates flight duration
    \param inParams No description given
*/
double func_t_flight(InputParameters &inParams);

/** \brief Calculates landing position
    \param inParams No description given
*/
double func_p_land(InputParameters &inParams);

/** \brief Calculates distance between the target position and the landing position
    \param inParams No description given
    \param p_land landing position
*/
double func_d_offset(InputParameters &inParams, double p_land);

/** \brief Calculates output message as a string
    \param inParams No description given
    \param d_offset distance between the target position and the landing position
*/
string func_s(InputParameters &inParams, double d_offset);

#endif
