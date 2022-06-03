#include "ODE.hpp"

#include <vector>

using std::vector;

ODE::ODE(double K_p, double K_d, double r_t) : K_p(K_p), K_d(K_d), r_t(r_t) {
}

void ODE::operator()(vector<double> y_t, vector<double> &dy_t, double t) {
    dy_t.at(0) = y_t.at(1);
    dy_t.at(1) = -(1.0 + K_d) * y_t.at(1) + -(20.0 + K_p) * y_t.at(0) + r_t * K_p;
}
