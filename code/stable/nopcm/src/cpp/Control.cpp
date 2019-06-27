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

#include "InputFormat.hpp"
#include "InputParameters.hpp"
#include "OutputFormat.hpp"

int main(int argc, const char *argv[]) {
    string filename = argv[1];
    double inParams.A_C;
    double inParams.C_W;
    double inParams.h_C;
    double inParams.T_init;
    double inParams.t_final;
    double inParams.L;
    double inParams.T_C;
    double inParams.t_step;
    double inParams.rho_W;
    double inParams.D;
    double inParams.A_tol;
    double inParams.R_tol;
    double inParams.T_W;
    double inParams.E_W;
    get_input(filename, A_C, C_W, h_C, T_init, t_final, L, T_C, t_step, rho_W, D, A_tol, R_tol, T_W, E_W);
    input_constraints(A_C, C_W, h_C, T_init, t_final, L, T_C, t_step, rho_W, D, T_W, E_W);
    write_output(T_W, E_W);
    return 0;
}

