/** \file Control.cpp
    \author Thulasi Jegatheesan
    \brief Controls the flow of the program
*/
#include <string>

#include "OutputFormat.hpp"
#include "InputParameters.hpp"

using std::string;

/** \brief Controls the flow of the program
    \param argc Number of command-line arguments
    \param argv List of command-line arguments
    \return exit code
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
