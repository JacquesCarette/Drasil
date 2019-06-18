#ifndef InputFormat_h
#define InputFormat_h

#include <string>
#include <vector>

using std::string;
using std::vector;
using std::ifstream;
using std::ofstream;

void func_get_input(string filename, double L, double D, double A_C, double T_C, double rho_W, double C_W, double h_C, double T_init, double t_step, double t_final, double A_tol, double R_tol);

#endif
