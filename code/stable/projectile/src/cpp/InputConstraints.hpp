/** \file InputConstraints.hpp
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
    \param inParams No description given
*/
void input_constraints(InputParameters &inParams);

#endif
