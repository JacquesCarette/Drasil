#include "Constants.hpp"

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

Constants::Constants() {
    L_min = 0.1;
    L_max = 50;
    rho_W_min = 950;
    rho_W_max = 1000;
    A_C_max = 100000;
    C_W_min = 4170;
    C_W_max = 4210;
    h_C_min = 10;
    h_C_max = 10000;
    t_final_max = 86400;
    AR_min = 1.0e-2;
    AR_max = 100;
}

Constants::~Constants() {
}

