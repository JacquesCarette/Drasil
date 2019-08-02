/** \file Control.cpp
    \brief Controls the flow of the program
*/
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
#include "OutputFormat.hpp"

/** \brief Controls the flow of the program
    \param argc Number of command-line arguments
    \param argv List of command-line arguments
*/
int main(int argc, const char *argv[]) {
    string filename = argv[1];
    double A_C;
    double C_W;
    double h_C;
    double T_init;
    double t_final;
    double L;
    double T_C;
    double t_step;
    double rho_W;
    double D;
    double A_tol;
    double R_tol;
    double T_W;
    double E_W;
    get_input(filename, A_C, C_W, h_C, T_init, t_final, L, T_C, t_step, rho_W, D, A_tol, R_tol, T_W, E_W);
    input_constraints(A_C, C_W, h_C, T_init, t_final, L, T_C, t_step, rho_W, D, T_W, E_W);
    write_output(T_W, E_W);
    
    return 0;
}

