/** \file InputConstraints.hpp
    \author Nikitha Krithnan and W. Spencer Smith
    \brief Provides the function for checking the physical constraints and software constraints on the input
*/
#ifndef InputConstraints_h
#define InputConstraints_h

#include <string>

#include "InputParameters.hpp"

using std::ofstream;
using std::string;

/** \brief Verifies that input values satisfy the physical constraints and software constraints
    \param inParams structure holding the input values
*/
void input_constraints(InputParameters &inParams);

#endif
