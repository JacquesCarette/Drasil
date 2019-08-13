/** \file OutputFormat.hpp
    \author Nikitha Krithnan and W. Spencer Smith
    \brief Provides the function for writing outputs
*/
#ifndef OutputFormat_h
#define OutputFormat_h

#include <string>
#include <vector>

using std::string;
using std::vector;
using std::ifstream;
using std::ofstream;

/** \brief Writes the output values to output.txt
    \param is_safePb variable that is assigned true when calculated probability is less than tolerable probability
    \param is_safeLR variable that is assigned true when load resistance (capacity) is greater than load (demand)
    \param P_b probability of breakage: The fraction of glass lites or plies that would break at the first occurrence of a specified load and duration, typically expressed in lites per 1000 (Ref: astm2016).
*/
void write_output(bool is_safePb, bool is_safeLR, double P_b);

#endif
