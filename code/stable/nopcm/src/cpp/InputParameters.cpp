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

void input_constraints(double A_C, double C_W, double h_C, double T_init, double t_final, double L, double T_C, double t_step, double rho_W, double D, double T_W, double E_W) {
    if (!((A_C <= 100000))) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!(((4170 < C_W) && (C_W < 4210)))) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!(((10 <= h_C) && (h_C <= 10000)))) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!((t_final < 86400))) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!(((0.1 <= L) && (L <= 50)))) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!(((950 < rho_W) && (rho_W <= 1000)))) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    
    if (!((A_C > 0))) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!((C_W > 0))) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!((h_C > 0))) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!(((0 < T_init) && (T_init < 100)))) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!((t_final > 0))) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!((L > 0))) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!(((0 < T_C) && (T_C < 100)))) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!(((0 < t_step) && (t_step < t_final)))) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!((rho_W > 0))) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!((D > 0))) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!(((T_init <= T_W) && (T_W <= T_C)))) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
    if (!((E_W >= 0))) {
        std::cout << "Warning: constraint violated" << std::endl;
    }
}

