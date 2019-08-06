#include "InputConstraints.hpp"

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

#include "InputParameters.hpp"

void input_constraints(InputParameters &inParams) {
    if (!(inParams.v_launch > 0)) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!(0 < inParams.theta && inParams.theta < 3.14159265 / 2)) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!(inParams.p_target > 0)) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
}

