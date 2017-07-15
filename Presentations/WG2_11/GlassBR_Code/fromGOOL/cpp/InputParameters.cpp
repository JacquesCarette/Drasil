#include "InputParameters.hpp"


#include <algorithm>
#include <iostream>
#include <fstream>
#include <iterator>
#include <string>
#include <math.h>
#include <sstream>
#include <vector>

using namespace GlassBR;
using std::string;
using std::vector;
using std::ifstream;
using std::ofstream;

InputParameters::InputParameters() {
    a = 0.0;
    b = 0.0;
    t = 2.5;
    gt = 1;
    w = 0.0;
    tnt = 0.0;
    sdx = 0.0;
    sdy = 0.0;
    sdz = 0.0;
    pbtol = 0.0;
    asprat = 0.0;
    sd = 0.0;
    h = 0.0;
    gtf = 0.0;
    ldf = 0.0;
    wtnt = 0.0;
    E = 7.17 * (pow(10.0, 7.0));
    td = 3.0;
    m = 7.0;
    k = 2.86 * (pow(10.0, -53.0));
    lsf = 1.0;
}

InputParameters::~InputParameters() {
}

