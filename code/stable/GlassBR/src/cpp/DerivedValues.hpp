/** \file DerivedValues.hpp
    \author Nikitha Krithnan and W. Spencer Smith
    \brief Provides the function for calculating derived values
*/
#ifndef DerivedValues_h
#define DerivedValues_h

#include <string>

#include "InputParameters.hpp"

using std::ofstream;
using std::string;

/** \brief Calculates values that can be immediately derived from the inputs
    \param inParams structure holding the input values
*/
void derived_values(InputParameters &inParams);

#endif
