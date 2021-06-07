/** \file InputFormat.hpp
    \author Nikitha Krithnan and W. Spencer Smith
    \brief Provides the function for reading inputs
*/
#ifndef InputFormat_h
#define InputFormat_h

#include <string>

#include "InputParameters.hpp"

using std::ifstream;
using std::ofstream;
using std::string;

/** \brief Reads input from a file with the given file name
    \param filename name of the input file
    \param inParams structure holding the input values
*/
void get_input(string filename, InputParameters &inParams);

#endif
