/** \file OutputFormat.hpp
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
    \param s output message as a string
    \param d_offset distance between the target position and the landing position
*/
void write_output(string s, double d_offset);

#endif
