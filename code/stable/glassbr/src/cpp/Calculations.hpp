#include "Interpolation.hpp"
#include "InputParameters.hpp"
#ifndef Calculations_h
#define Calculations_h

#include <string>
#include <vector>

using std::string;
using std::vector;
using std::ifstream;
using std::ofstream;



double func_q(InputParameters &inParams);

bool func_is_safePb(InputParameters &inParams, double P_b);

bool func_is_safeLR(double LR, double q);

double func_B(InputParameters &inParams, double J);

double func_J(InputParameters &inParams, double q_hat);

double func_NFL(InputParameters &inParams, double q_hat_tol);

double func_q_hat(InputParameters &inParams, double q);

double func_q_hat_tol(InputParameters &inParams, double J_tol);

double func_J_tol(InputParameters &inParams);

double func_P_b(double B);

double func_LR(InputParameters &inParams, double NFL);

double func_q(InputParameters &inParams);

#endif
