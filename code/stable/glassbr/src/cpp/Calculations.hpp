/** \file Calculations.hpp
    \author Nikitha Krithnan and W. Spencer Smith
    \brief Provides functions for calculating the outputs
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
#include "Interpolation.hpp"

/** \brief Calculates stress distribution factor (Function) based on Pbtol
    \param inParams structure holding the input values
    \return stress distribution factor (Function) based on Pbtol
*/
double func_J_tol(InputParameters &inParams);

/** \brief Calculates applied load (demand): 3 second duration equivalent pressure (Pa)
    \param inParams structure holding the input values
    \return applied load (demand): 3 second duration equivalent pressure (Pa)
*/
double func_q(InputParameters &inParams);

/** \brief Calculates dimensionless load
    \param inParams structure holding the input values
    \param q applied load (demand): 3 second duration equivalent pressure (Pa)
    \return dimensionless load
*/
double func_q_hat(InputParameters &inParams, double q);

/** \brief Calculates tolerable load
    \param inParams structure holding the input values
    \param J_tol stress distribution factor (Function) based on Pbtol
    \return tolerable load
*/
double func_q_hat_tol(InputParameters &inParams, double J_tol);

/** \brief Calculates stress distribution factor (Function)
    \param inParams structure holding the input values
    \param q_hat dimensionless load
    \return stress distribution factor (Function)
*/
double func_J(InputParameters &inParams, double q_hat);

/** \brief Calculates non-factored load: three second duration uniform load associated with a probability of breakage less than or equal to 8 lites per 1000 for monolithic AN glass (Pa)
    \param inParams structure holding the input values
    \param q_hat_tol tolerable load
    \return non-factored load: three second duration uniform load associated with a probability of breakage less than or equal to 8 lites per 1000 for monolithic AN glass (Pa)
*/
double func_NFL(InputParameters &inParams, double q_hat_tol);

/** \brief Calculates risk of failure
    \param inParams structure holding the input values
    \param J stress distribution factor (Function)
    \return risk of failure
*/
double func_B(InputParameters &inParams, double J);

/** \brief Calculates load resistance: the uniform lateral load that a glass construction can sustain based upon a given probability of breakage and load duration as defined in (pp. 1 and 53) Ref: astm2009 (Pa)
    \param inParams structure holding the input values
    \param NFL non-factored load: three second duration uniform load associated with a probability of breakage less than or equal to 8 lites per 1000 for monolithic AN glass (Pa)
    \return load resistance: the uniform lateral load that a glass construction can sustain based upon a given probability of breakage and load duration as defined in (pp. 1 and 53) Ref: astm2009 (Pa)
*/
double func_LR(InputParameters &inParams, double NFL);

/** \brief Calculates 3 second load equivalent resistance safety requirement
    \param LR load resistance: the uniform lateral load that a glass construction can sustain based upon a given probability of breakage and load duration as defined in (pp. 1 and 53) Ref: astm2009 (Pa)
    \param q applied load (demand): 3 second duration equivalent pressure (Pa)
    \return 3 second load equivalent resistance safety requirement
*/
bool func_is_safeLR(double LR, double q);

/** \brief Calculates probability of breakage: the fraction of glass lites or plies that would break at the first occurrence of a specified load and duration, typically expressed in lites per 1000 (Ref: astm2016)
    \param B risk of failure
    \return probability of breakage: the fraction of glass lites or plies that would break at the first occurrence of a specified load and duration, typically expressed in lites per 1000 (Ref: astm2016)
*/
double func_P_b(double B);

/** \brief Calculates probability of glass breakage safety requirement
    \param inParams structure holding the input values
    \param P_b probability of breakage: the fraction of glass lites or plies that would break at the first occurrence of a specified load and duration, typically expressed in lites per 1000 (Ref: astm2016)
    \return probability of glass breakage safety requirement
*/
bool func_is_safePb(InputParameters &inParams, double P_b);

#endif
