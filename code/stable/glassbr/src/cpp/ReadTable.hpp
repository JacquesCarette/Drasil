/** \file ReadTable.hpp
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
    \param filename No description given
    \param z_vector No description given
    \param x_matrix No description given
    \param y_matrix No description given
*/
void func_read_table(string filename, vector<double> &z_vector, vector<vector<double>> &x_matrix, vector<vector<double>> &y_matrix);

#endif
