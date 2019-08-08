/** \file Calculations.hpp
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
*/
double func_J_tol(InputParameters &inParams);

/** \brief Calculates applied load (demand)
    \param inParams structure holding the input values
*/
double func_q(InputParameters &inParams);

/** \brief Calculates dimensionless load
    \param inParams structure holding the input values
    \param q applied load (demand) (Pa)
*/
double func_q_hat(InputParameters &inParams, double q);

/** \brief Calculates tolerable load
    \param inParams structure holding the input values
    \param J_tol stress distribution factor (Function) based on Pbtol
*/
double func_q_hat_tol(InputParameters &inParams, double J_tol);

/** \brief Calculates stress distribution factor (Function)
    \param inParams structure holding the input values
    \param q_hat dimensionless load
*/
double func_J(InputParameters &inParams, double q_hat);

/** \brief Calculates non-factored load
    \param inParams structure holding the input values
    \param q_hat_tol tolerable load
*/
double func_NFL(InputParameters &inParams, double q_hat_tol);

/** \brief Calculates risk of failure
    \param inParams structure holding the input values
    \param J stress distribution factor (Function)
*/
double func_B(InputParameters &inParams, double J);

/** \brief Calculates load resistance
    \param inParams structure holding the input values
    \param NFL non-factored load (Pa)
*/
double func_LR(InputParameters &inParams, double NFL);

/** \brief Calculates variable that is assigned true when load resistance (capacity) is greater than load (demand)
    \param LR load resistance (Pa)
    \param q applied load (demand) (Pa)
*/
bool func_is_safeLR(double LR, double q);

/** \brief Calculates probability of breakage
    \param B risk of failure
*/
double func_P_b(double B);

/** \brief Calculates variable that is assigned true when calculated probability is less than tolerable probability
    \param inParams structure holding the input values
    \param P_b probability of breakage
*/
bool func_is_safePb(InputParameters &inParams, double P_b);

#endif
