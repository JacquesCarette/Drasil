#ifndef InputParameters_h
#define InputParameters_h

#include <string>
#include <vector>

using std::string;
using std::vector;
using std::ifstream;
using std::ofstream;

void get_input(string filename, double &A_C, double &C_W, double &h_C, double &T_i, double &t_final, double &L, double &T_C, double &t_step, double &rho_W, double &D, double &A_tol, double &R_tol, double &T_W, double &E_W);

void input_constraints(double A_C, double C_W, double h_C, double T_i, double t_final, double L, double T_C, double t_step, double rho_W, double D, double T_W, double E_W);

#endif
