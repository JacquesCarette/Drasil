/** \file Interpolation.hpp
    \author Nikitha Krithnan and W. Spencer Smith
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
    \param x_1 lower x-coordinate
    \param y_1 lower y-coordinate
    \param x_2 upper x-coordinate
    \param y_2 upper y-coordinate
    \param x x-coordinate to interpolate at
*/
double func_lin_interp(double x_1, double y_1, double x_2, double y_2, double x);

/** \brief Finds the array index for a value closest to the given value
    \param arr array in which value should be found
    \param v value whose index will be found
*/
int func_find(vector<double> &arr, double v);

/** \brief Extracts a column from a 2D matrix
    \param mat matrix from which column will be extracted
    \param j index
*/
vector<double> func_extractColumn(vector<vector<double>> &mat, int j);

/** \brief Linearly interpolates a y value at given x and z values
    \param filename name of file with x y and z data
    \param x x-coordinate to interpolate at
    \param z z-coordinate to interpolate at
*/
double func_interpY(string filename, double x, double z);

/** \brief Linearly interpolates a z value at given x and y values
    \param filename name of file with x y and z data
    \param x x-coordinate to interpolate at
    \param y y-coordinate to interpolate at
*/
double func_interpZ(string filename, double x, double y);

#endif
