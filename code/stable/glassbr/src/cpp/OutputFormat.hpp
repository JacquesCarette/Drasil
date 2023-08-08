/** \file OutputFormat.hpp
    \author Nikitha Krithnan and W. Spencer Smith
    \brief Provides the function for writing outputs
*/
#ifndef OutputFormat_h
#define OutputFormat_h

#include <string>

#include "InputParameters.hpp"

using std::ofstream;
using std::string;

/** \brief Writes the output values to output.txt
    \param inParams structure holding the input values
    \param B risk of failure
    \param J stress distribution factor (Function)
    \param NFL non-factored load: three second duration uniform load associated with a probability of breakage less than or equal to 8 lites per 1000 for monolithic AN glass (Pa)
    \param q_hat dimensionless load
    \param q_hat_tol tolerable load
    \param J_tol stress distribution factor (Function) based on Pbtol
    \param P_b probability of breakage: the fraction of glass lites or plies that would break at the first occurrence of a specified load and duration, typically expressed in lites per 1000 (Ref: astm2016)
    \param LR load resistance: the uniform lateral load that a glass construction can sustain based upon a given probability of breakage and load duration as defined in (pp. 1 and 53) Ref: astm2009 (Pa)
    \param isSafePb Safety Req-Pb
    \param isSafeLR Safety Req-LR
*/
void write_output(InputParameters &inParams, double B, double J, double NFL, double q_hat, double q_hat_tol, double J_tol, double P_b, double LR, bool isSafePb, bool isSafeLR);

#endif
