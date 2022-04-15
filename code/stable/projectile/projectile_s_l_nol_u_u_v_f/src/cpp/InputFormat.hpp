/** \file InputFormat.hpp
    \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
    \brief Provides the function for reading inputs
*/
#ifndef InputFormat_h
#define InputFormat_h

#include <string>

using std::ifstream;
using std::string;

/** \brief Reads input from a file with the given file name
    \param filename name of the input file
    \param v_launch launch speed: the initial speed of the projectile when launched (m/s)
    \param theta launch angle: the angle between the launcher and a straight line from the launcher to the target (rad)
    \param p_target target position: the distance from the launcher to the target (m)
*/
void get_input(string filename, float &v_launch, float &theta, float &p_target);

#endif
