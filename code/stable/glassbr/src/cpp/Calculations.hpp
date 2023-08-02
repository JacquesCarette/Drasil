/** \file Calculations.hpp
    \author Nikitha Krithnan and W. Spencer Smith
    \brief Provides functions for calculating the outputs
*/
#ifndef Calculations_h
#define Calculations_h

#include <string>

#include "InputParameters.hpp"

using std::ofstream;
using std::string;

/** \brief Calculates minimum thickness (m)
    \param inParams structure holding the input values
    \return minimum thickness (m)
*/
double func_h(InputParameters &inParams);

/** \brief Calculates glass type factor: a multiplying factor for adjusting the LR of different glass type, that is, AN, FT, or HS, in monolithic glass, LG (Laminated Glass), or IG (Insulating Glass) constructions
    \param inParams structure holding the input values
    \return glass type factor: a multiplying factor for adjusting the LR of different glass type, that is, AN, FT, or HS, in monolithic glass, LG (Laminated Glass), or IG (Insulating Glass) constructions
*/
int func_GTF(InputParameters &inParams);

/** \brief Calculates aspect ratio: the ratio of the long dimension of the glass to the short dimension of the glass. For glass supported on four sides, the aspect ratio is always equal to or greater than 1.0. For glass supported on three sides, the ratio of the length of one of the supported edges perpendicular to the free edge, to the length of the free edge, is equal to or greater than 0.5
    \param inParams structure holding the input values
    \return aspect ratio: the ratio of the long dimension of the glass to the short dimension of the glass. For glass supported on four sides, the aspect ratio is always equal to or greater than 1.0. For glass supported on three sides, the ratio of the length of one of the supported edges perpendicular to the free edge, to the length of the free edge, is equal to or greater than 0.5
*/
double func_AR(InputParameters &inParams);

/** \brief Calculates applied load (demand): 3 second duration equivalent pressure (Pa)
    \param inParams structure holding the input values
    \return applied load (demand): 3 second duration equivalent pressure (Pa)
*/
double func_q(InputParameters &inParams);

/** \brief Calculates dimensionless load
    \param inParams structure holding the input values
    \param q applied load (demand): 3 second duration equivalent pressure (Pa)
    \param h minimum thickness (m)
    \param GTF glass type factor: a multiplying factor for adjusting the LR of different glass type, that is, AN, FT, or HS, in monolithic glass, LG (Laminated Glass), or IG (Insulating Glass) constructions
    \return dimensionless load
*/
double func_q_hat(InputParameters &inParams, double q, double h, int GTF);

/** \brief Calculates stress distribution factor (Function) based on Pbtol
    \param inParams structure holding the input values
    \param h minimum thickness (m)
    \return stress distribution factor (Function) based on Pbtol
*/
double func_J_tol(InputParameters &inParams, double h);

/** \brief Calculates stress distribution factor (Function)
    \param AR aspect ratio: the ratio of the long dimension of the glass to the short dimension of the glass. For glass supported on four sides, the aspect ratio is always equal to or greater than 1.0. For glass supported on three sides, the ratio of the length of one of the supported edges perpendicular to the free edge, to the length of the free edge, is equal to or greater than 0.5
    \param q_hat dimensionless load
    \return stress distribution factor (Function)
*/
double func_J(double AR, double q_hat);

/** \brief Calculates tolerable load
    \param AR aspect ratio: the ratio of the long dimension of the glass to the short dimension of the glass. For glass supported on four sides, the aspect ratio is always equal to or greater than 1.0. For glass supported on three sides, the ratio of the length of one of the supported edges perpendicular to the free edge, to the length of the free edge, is equal to or greater than 0.5
    \param J_tol stress distribution factor (Function) based on Pbtol
    \return tolerable load
*/
double func_q_hat_tol(double AR, double J_tol);

/** \brief Calculates risk of failure
    \param inParams structure holding the input values
    \param h minimum thickness (m)
    \param J stress distribution factor (Function)
    \return risk of failure
*/
double func_B(InputParameters &inParams, double h, double J);

/** \brief Calculates non-factored load: three second duration uniform load associated with a probability of breakage less than or equal to 8 lites per 1000 for monolithic AN glass (Pa)
    \param inParams structure holding the input values
    \param q_hat_tol tolerable load
    \param h minimum thickness (m)
    \return non-factored load: three second duration uniform load associated with a probability of breakage less than or equal to 8 lites per 1000 for monolithic AN glass (Pa)
*/
double func_NFL(InputParameters &inParams, double q_hat_tol, double h);

/** \brief Calculates probability of breakage: the fraction of glass lites or plies that would break at the first occurrence of a specified load and duration, typically expressed in lites per 1000 (Ref: astm2016)
    \param B risk of failure
    \return probability of breakage: the fraction of glass lites or plies that would break at the first occurrence of a specified load and duration, typically expressed in lites per 1000 (Ref: astm2016)
*/
double func_P_b(double B);

/** \brief Calculates load resistance: the uniform lateral load that a glass construction can sustain based upon a given probability of breakage and load duration as defined in (pp. 1 and 53) Ref: astm2009 (Pa)
    \param NFL non-factored load: three second duration uniform load associated with a probability of breakage less than or equal to 8 lites per 1000 for monolithic AN glass (Pa)
    \param GTF glass type factor: a multiplying factor for adjusting the LR of different glass type, that is, AN, FT, or HS, in monolithic glass, LG (Laminated Glass), or IG (Insulating Glass) constructions
    \return load resistance: the uniform lateral load that a glass construction can sustain based upon a given probability of breakage and load duration as defined in (pp. 1 and 53) Ref: astm2009 (Pa)
*/
double func_LR(double NFL, int GTF);

/** \brief Calculates probability of glass breakage safety requirement
    \param inParams structure holding the input values
    \param P_b probability of breakage: the fraction of glass lites or plies that would break at the first occurrence of a specified load and duration, typically expressed in lites per 1000 (Ref: astm2016)
    \return probability of glass breakage safety requirement
*/
bool func_isSafePb(InputParameters &inParams, double P_b);

/** \brief Calculates 3 second load equivalent resistance safety requirement
    \param LR load resistance: the uniform lateral load that a glass construction can sustain based upon a given probability of breakage and load duration as defined in (pp. 1 and 53) Ref: astm2009 (Pa)
    \param q applied load (demand): 3 second duration equivalent pressure (Pa)
    \return 3 second load equivalent resistance safety requirement
*/
bool func_isSafeLR(double LR, double q);

#endif
