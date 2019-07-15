/** \file Interpolation.hpp
    \brief Provides functions for linear interpolation on three-dimensional data
*/
#ifndef Interpolation_h
#define Interpolation_h

#include <string>
#include <vector>

using std::string;
using std::vector;
using std::ifstream;
using std::ofstream;

#include "ReadTable.hpp"

/** \brief Performs linear interpolation
    \param x_1 No description given
    \param y_1 No description given
    \param x_2 No description given
    \param y_2 No description given
    \param x No description given
*/
double func_lin_interp(double x_1, double y_1, double x_2, double y_2, double x);

/** \brief Finds the array index for a value closest to the given value
    \param arr No description given
    \param v No description given
*/
int func_find(vector<double> &arr, double v);

/** \brief Extracts a column from a 2D matrix
    \param mat No description given
    \param j No description given
*/
vector<double> func_extractColumn(vector<vector<double>> &mat, int j);

/** \brief Linearly interpolates a y value at given x and z values
    \param filename No description given
    \param x No description given
    \param z No description given
*/
double func_interpY(string filename, double x, double z);

/** \brief Linearly interpolates a z value at given x and y values
    \param filename No description given
    \param x No description given
    \param y No description given
*/
double func_interpZ(string filename, double x, double y);

#endif
