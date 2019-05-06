#include "Calculations.hpp"

#include "Interpolation.hpp"
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

double func_q(InputParameters &inParams) {
    return func_interpY("TSD.txt", inParams.SD, inParams.w_TNT);
}

bool func_is_safePb(InputParameters &inParams, double P_b) {
    return P_b < inParams.P_btol;
}

bool func_is_safeLR(double LR, double q) {
    return LR > q;
}

double func_B(InputParameters &inParams, double J) {
    return (2.86e-53 / (pow(inParams.a * inParams.b, 7.0 - 1))) * ((pow(7.17e10 * (pow(inParams.h, 2)), 7.0)) * (inParams.LDF * (exp(J))));
}

double func_J(InputParameters &inParams, double q_hat) {
    return func_interpZ("SDF.txt", inParams.AR, q_hat);
}

double func_NFL(InputParameters &inParams, double q_hat_tol) {
    return (q_hat_tol * (7.17e10 * (pow(inParams.h, 4)))) / (pow(inParams.a * inParams.b, 2));
}

double func_q_hat(InputParameters &inParams, double q) {
    return (q * (pow(inParams.a * inParams.b, 2))) / (7.17e10 * ((pow(inParams.h, 4)) * inParams.GTF));
}

double func_q_hat_tol(InputParameters &inParams, double J_tol) {
    return func_interpY("SDF.txt", inParams.AR, J_tol);
}

double func_J_tol(InputParameters &inParams) {
    return log((log(1 / (1 - inParams.P_btol))) * ((pow(inParams.a * inParams.b, 7.0 - 1)) / (2.86e-53 * ((pow(7.17e10 * (pow(inParams.h, 2)), 7.0)) * inParams.LDF))));
}

double func_P_b(double B) {
    return 1 - (exp(-(B)));
}

double func_LR(InputParameters &inParams, double NFL) {
    return NFL * (inParams.GTF * 1);
}

double func_q(InputParameters &inParams) {
    return func_interpY("TSD.txt", inParams.SD, inParams.w_TNT);
}

