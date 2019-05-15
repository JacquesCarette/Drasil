#include "InputParameters.hpp"
#ifndef InputFormat_h
#define InputFormat_h

#include <string>
#include <vector>

using std::string;
using std::vector;
using std::ifstream;
using std::ofstream;



void func_get_inputs(string filename, InputParameters &inParams, double τ, double A_tol, double R_tol, double C_tol);

#endif
