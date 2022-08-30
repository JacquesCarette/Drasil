/** \file OutputFormat.hpp
    \author Naveen Ganesh Muralidharan
    \brief Provides the function for writing outputs
*/
#ifndef OutputFormat_h
#define OutputFormat_h

#include <string>
#include <vector>

using std::ofstream;
using std::string;
using std::vector;

/** \brief Writes the output values to output.txt
    \param y_t Process Variable: The output value from the power plant
*/
void write_output(vector<double> &y_t);

#endif
