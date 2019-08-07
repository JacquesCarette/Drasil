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
        std::cout << "Warning: ";
        std::cout << "v_launch has value ";
        std::cout << inParams.v_launch;
        std::cout << " but suggested to be ";
        std::cout << "above ";
        std::cout << 0;
        std::cout << "." << std::endl;
    }
    if (!(0 < inParams.theta && inParams.theta < 3.14159265 / 2)) {
        std::cout << "Warning: ";
        std::cout << "theta has value ";
        std::cout << inParams.theta;
        std::cout << " but suggested to be ";
        std::cout << "between ";
        std::cout << 0;
        std::cout << " and ";
        std::cout << (3.14159265 / 2);
        std::cout << " ((pi)/(2))";
        std::cout << "." << std::endl;
    }
    if (!(inParams.p_target > 0)) {
        std::cout << "Warning: ";
        std::cout << "p_target has value ";
        std::cout << inParams.p_target;
        std::cout << " but suggested to be ";
        std::cout << "above ";
        std::cout << 0;
        std::cout << "." << std::endl;
    }
}

