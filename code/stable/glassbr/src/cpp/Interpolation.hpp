#include "ReadTable.hpp"
#ifndef Interpolation_h
#define Interpolation_h

#include <string>
#include <vector>

using std::string;
using std::vector;
using std::ifstream;
using std::ofstream;



double func_lin_interp(double x_1, double y_1, double x_2, double y_2, double x);

int func_find(vector<double> &arr, double v);

vector<double> func_extractColumn(vector< vector<double> > &mat, int j);

double func_interpY(string filename, double x, double z);

double func_interpZ(string filename, double x, double y);

#endif
