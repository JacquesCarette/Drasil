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

const double Constants::L_min = 0.1;
const double Constants::L_max = 50;
const double Constants::rho_W_min = 950;
const double Constants::rho_W_max = 1000;
const double Constants::A_C_max = 100000;
const double Constants::C_W_min = 4170;
const double Constants::C_W_max = 4210;
const double Constants::h_C_min = 10;
const double Constants::h_C_max = 10000;
const double Constants::t_final_max = 86400;
const double Constants::AR_min = 1.0e-2;
const double Constants::AR_max = 100;

Constants::~Constants() {
}
