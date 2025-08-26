#include "ODE.hpp"

#include <vector>

using std::vector;

ODE::ODE(double tau_W, double T_C) : tau_W(tau_W), T_C(T_C) {
}

void ODE::operator()(vector<double> T_W, vector<double> &dT_W, double t) {
    dT_W.at(0) = -(1.0 / tau_W) * T_W.at(0) + 1.0 / tau_W * T_C;
}
