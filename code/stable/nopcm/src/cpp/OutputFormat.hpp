/** \file OutputFormat.hpp
    \author Thulasi Jegatheesan
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
    \param T_W temperature of the water (degreeC)
    \param E_W change in heat energy in the water (J)
*/
void write_output(double T_W, double E_W);

#endif
