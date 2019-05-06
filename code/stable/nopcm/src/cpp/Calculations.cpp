#include "Calculations.hpp"

#include "InputParameters.hpp"

#include <algorithm>
#include <iostream>
#include <fstream>
#include <iterator>
#include <string>
#include <math.h>
#include <sstream>
#include <limits>
#include <vector>

using std::string;
using std::vector;
using std::ifstream;
using std::ofstream;

double func_q_C(InputParameters &inParams, double t) {
    return inParams.h_C * (inParams.T_C - func_T_W(t));
}

double func_q_C(InputParameters &inParams, double t) {
    return inParams.h_C * (inParams.T_C - func_T_W(t));
}

