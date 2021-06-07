/** \file Control.cpp
    \author Thulasi Jegatheesan
    \brief Controls the flow of the program
*/
#include <string>
#include <vector>

#include "Calculations.hpp"
#include "InputParameters.hpp"
#include "OutputFormat.hpp"

using std::string;
using std::vector;

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
    double E_W;
    double V_tank;
    get_input(filename, A_C, C_W, h_C, T_init, t_final, L, T_C, t_step, rho_W, D, A_tol, R_tol, E_W);
    V_tank = derived_values(D, L);
    input_constraints(A_C, C_W, h_C, T_init, t_final, L, T_C, t_step, rho_W, D, E_W);
    double V_W = func_V_W(V_tank);
    double m_W = func_m_W(rho_W, V_W);
    double tau_W = func_tau_W(C_W, h_C, A_C, m_W);
    vector<double> T_W = func_T_W(T_C, t_final, T_init, A_tol, R_tol, t_step, tau_W);
    write_output(E_W, T_W);
    
    return 0;
}
