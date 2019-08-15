/** \file ReadTable.hpp
    \author Nikitha Krithnan and W. Spencer Smith
    \brief Provides a function for reading glass ASTM data
*/
#ifndef ReadTable_h
#define ReadTable_h

#include <string>
#include <vector>

using std::string;
using std::vector;
using std::ifstream;
using std::ofstream;

/** \brief Reads glass ASTM data from a file with the given file name
    \param filename name of the input file
    \param z_vector list of z values
    \param x_matrix lists of x values at different z values
    \param y_matrix lists of y values at different z values
*/
void func_read_table(string filename, vector<double> &z_vector, vector<vector<double>> &x_matrix, vector<vector<double>> &y_matrix);

#endif
