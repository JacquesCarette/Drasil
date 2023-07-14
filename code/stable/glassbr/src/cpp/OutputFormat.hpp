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
    \param isSafePb Safety Req-Pb
    \param isSafeLR Safety Req-LR
    \param B risk of failure
    \param J stress distribution factor (Function)
    \param NFL non-factored load: three second duration uniform load associated with a probability of breakage less than or equal to 8 lites per 1000 for monolithic AN glass (Pa)
    \param q_hat dimensionless load
    \param q_hat_tol tolerable load
    \param J_tol stress distribution factor (Function) based on Pbtol
*/
void write_output(InputParameters &inParams, bool isSafePb, bool isSafeLR, double B, double J, double NFL, double q_hat, double q_hat_tol, double J_tol);

#endif
