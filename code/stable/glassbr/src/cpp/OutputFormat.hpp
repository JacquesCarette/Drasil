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
    \param is_safePb probability of glass breakage safety requirement
    \param is_safeLR load resistance safety requirement
    \param P_b probability of breakage
*/
void write_output(bool is_safePb, bool is_safeLR, double P_b);

#endif
