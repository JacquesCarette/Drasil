/** \file OutputFormat.hpp
    \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
    \brief Provides the function for writing outputs
*/
#ifndef OutputFormat_h
#define OutputFormat_h

#include <string>

using std::ofstream;
using std::string;

/** \brief Writes the output values to output.txt
    \param s output message as a string
    \param d_offset distance between the target position and the landing position: the offset between the target position and the landing position (m)
    \param t_flight flight duration: the time when the projectile lands (s)
*/
void write_output(string s, float d_offset, float t_flight);

#endif
