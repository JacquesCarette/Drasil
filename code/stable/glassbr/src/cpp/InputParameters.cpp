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

InputParameters::InputParameters() {
    a = 0.0;
    b = 0.0;
    SD = 0.0;
    w = 0.0;
    AR = 0.0;
    P_btol = 0.0;
    TNT = 0.0;
    g = "";
    t = 0.0;
    SD_x = 0.0;
    SD_y = 0.0;
    SD_z = 0.0;
    h = 0.0;
    LDF = 0.0;
    GTF = 0.0;
    w_TNT = 0.0;
}

InputParameters::~InputParameters() {
}

