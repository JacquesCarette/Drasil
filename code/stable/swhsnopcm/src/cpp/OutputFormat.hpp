/** \file OutputFormat.hpp
    \author Thulasi Jegatheesan
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
    \param E_W change in heat energy in the water: change in thermal energy within the water (J)
    \param T_W temperature of the water: the average kinetic energy of the particles within the water (degreeC)
*/
void write_output(double E_W, vector<double> &T_W);

#endif
