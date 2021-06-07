/** \file Interpolation.hpp
    \author Nikitha Krithnan and W. Spencer Smith
    \brief Provides functions for linear interpolation on three-dimensional data
*/
#ifndef Interpolation_h
#define Interpolation_h

#include <string>
#include <vector>

using std::ofstream;
using std::string;
using std::vector;

/** \brief Performs linear interpolation
    \param x_1 lower x-coordinate
    \param y_1 lower y-coordinate
    \param x_2 upper x-coordinate
    \param y_2 upper y-coordinate
    \param x x-coordinate to interpolate at
    \return y value interpolated at given x value
*/
double lin_interp(double x_1, double y_1, double x_2, double y_2, double x);

/** \brief Finds the array index for a value closest to the given value
    \param arr array in which value should be found
    \param v value whose index will be found
    \return index of given value in given array
*/
int find(vector<double> &arr, double v);

/** \brief Extracts a column from a 2D matrix
    \param mat matrix from which column will be extracted
    \param j index
    \return column of the given matrix at the given index
*/
vector<double> extractColumn(vector<vector<double>> &mat, int j);

/** \brief Linearly interpolates a y value at given x and z values
    \param filename name of file with x y and z data
    \param x x-coordinate to interpolate at
    \param z z-coordinate to interpolate at
    \return y value interpolated at given x and z values
*/
double interpY(string filename, double x, double z);

/** \brief Linearly interpolates a z value at given x and y values
    \param filename name of file with x y and z data
    \param x x-coordinate to interpolate at
    \param y y-coordinate to interpolate at
    \return z value interpolated at given x and y values
*/
double interpZ(string filename, double x, double y);

#endif
