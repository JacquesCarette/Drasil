#ifndef InputFormat_h
#define InputFormat_h

#include <string>
#include <vector>

using std::string;
using std::vector;
using std::ifstream;
using std::ofstream;

#include "InputParameters.hpp"

void func_get_input(string filename, InputParameters &inParams, double τ, double A_tol, double R_tol);

#endif
