/** \file InputConstraints.hpp
    \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
    \brief Provides the function for checking the physical constraints and software constraints on the input
*/
#ifndef InputConstraints_h
#define InputConstraints_h

#include <string>
#include <vector>

using std::string;
using std::vector;
using std::ifstream;
using std::ofstream;

#include "InputParameters.hpp"

/** \brief Verifies that input values satisfy the physical constraints and software constraints
    \param inParams structure holding the input values
*/
void input_constraints(InputParameters &inParams);

#endif
