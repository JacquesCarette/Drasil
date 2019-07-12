#ifndef Calculations_h
#define Calculations_h

#include <string>
#include <vector>

using std::string;
using std::vector;
using std::ifstream;
using std::ofstream;

#include "InputParameters.hpp"

double func_t_flight(InputParameters &inParams);

double func_p_land(InputParameters &inParams);

double func_d_offset(InputParameters &inParams, double p_land);

string func_s(InputParameters &inParams, double d_offset);

#endif
