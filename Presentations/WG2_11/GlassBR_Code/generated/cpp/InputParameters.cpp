#include "InputParameters.hpp"


#include <algorithm>
#include <iostream>
#include <fstream>
#include <iterator>
#include <string>
#include <math.h>
#include <sstream>
#include <vector>

using namespace GlassBR_program;
using std::string;
using std::vector;
using std::ifstream;
using std::ofstream;

void GlassBR_program::get_inputs(string filename) {
    ifstream infile;
    infile.open(filename, std::fstream::in);
    infile >> a;
    infile >> b;
    infile >> w;
    infile >> SD;
    infile >> P_btol;
    infile >> TNT;
    infile >> g;
    infile >> t;
    infile.close();
}

void GlassBR_program::input_constraints() {
    if (!(d_min <= a)) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!(a <= d_max)) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!((a / b) < AR_max)) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!(d_min <= b)) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!(b <= d_max)) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!((a / b) < AR_max)) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!(w_max <= w)) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!(w <= w_min)) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!(SD_min < SD)) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!(SD < SD_max)) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!(a > 0.0)) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!((a / b) > 1.0)) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!(b > 0.0)) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!(b < a)) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!(w >= 0.0)) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!(SD > 0.0)) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!(0.0 < P_btol)) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!(P_btol < 1.0)) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!(TNT > 0.0)) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
}

