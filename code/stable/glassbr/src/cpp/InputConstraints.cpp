#include "InputConstraints.hpp"

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

void input_constraints(InputParameters &inParams) {
    if (!((0.1 <= inParams.a) && (inParams.a <= 5.0))) {
        throw("InputError");
    }
    if (!((0.1 <= inParams.b) && (inParams.b <= 5.0))) {
        throw("InputError");
    }
    if (!((6.0 <= inParams.SD) && (inParams.SD <= 130.0))) {
        throw("InputError");
    }
    if (!((4.5 <= inParams.w) && (inParams.w <= 910.0))) {
        throw("InputError");
    }
    if (!(inParams.AR <= 5.0)) {
        throw("InputError");
    }
    if (!(inParams.a > 0)) {
        throw("InputError");
    }
    if (!(inParams.a >= inParams.b)) {
        throw("InputError");
    }
    if (!((0 < inParams.b) && (inParams.b <= inParams.a))) {
        throw("InputError");
    }
    if (!(inParams.SD > 0)) {
        throw("InputError");
    }
    if (!(inParams.w > 0)) {
        throw("InputError");
    }
    if (!(inParams.AR >= 1)) {
        throw("InputError");
    }
    if (!((0 < inParams.P_btol) && (inParams.P_btol < 1))) {
        throw("InputError");
    }
    if (!(inParams.TNT > 0)) {
        throw("InputError");
    }
}

