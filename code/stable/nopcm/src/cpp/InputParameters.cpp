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
    if (!(inParams.A_C <= A_C_max)) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!((C_W_min < inParams.C_W) && (inParams.C_W < C_W_max))) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!((h_C_min <= inParams.h_C) && (inParams.h_C <= h_C_max))) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!(inParams.t_final < t_final_max)) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!((L_min <= inParams.L) && (inParams.L <= L_max))) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!((ρ_W_min < inParams.ρ_W) && (inParams.ρ_W <= ρ_W_max))) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!(inParams.A_C > 0)) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!(inParams.C_W > 0)) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!(inParams.h_C > 0)) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!((0 < inParams.T_init) && (inParams.T_init < 100))) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!(inParams.t_final > 0)) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!(inParams.L > 0)) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!((0 < inParams.T_C) && (inParams.T_C < 100))) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!(inParams.ρ_W > 0)) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!(inParams.D > 0)) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!((inParams.T_init <= inParams.T_W) && (inParams.T_W <= inParams.T_C))) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!(inParams.E_W >= 0)) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
}

