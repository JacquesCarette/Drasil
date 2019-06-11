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

void derived_values() {
}

void input_constraints(InputParameters &inParams) {
    if (!((inParams.A_C <= 100000))) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!(((4170 < inParams.C_W) && (inParams.C_W < 4210)))) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!(((10 <= inParams.h_C) && (inParams.h_C <= 10000)))) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!((inParams.t_final < 86400))) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!(((0.1 <= inParams.L) && (inParams.L <= 50)))) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!(((950 < inParams.rho_W) && (inParams.rho_W <= 1000)))) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!(((1.0e-2 <= inParams.D) && (inParams.D <= 100)))) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    
    if (!((inParams.A_C > 0))) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!((inParams.C_W > 0))) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!((inParams.h_C > 0))) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!(((0 < inParams.T_init) && (inParams.T_init < 100)))) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!((inParams.t_final > 0))) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!((inParams.L > 0))) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!(((0 < inParams.T_C) && (inParams.T_C < 100)))) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!(((0 < inParams.t_step) && (inParams.t_step < inParams.t_final)))) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!((inParams.rho_W > 0))) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!((inParams.D > 0))) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!(((inParams.T_init <= inParams.T_W) && (inParams.T_W <= inParams.T_C)))) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!((inParams.E_W >= 0))) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
}

