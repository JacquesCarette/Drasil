#include "T_ODE.hpp"

#include <vector>

using std::vector;

T_ODE::T_ODE(double c) : c(c) {
}

void T_ODE::operator()(double T, float &dTdt, double t) {
    dTdt = T + c;
}

Populate_T::Populate_T(vector<double> &T) : T(T) {
}

void Populate_T::operator()(double &T, double t) {
    this->T.push_back(T);
}
