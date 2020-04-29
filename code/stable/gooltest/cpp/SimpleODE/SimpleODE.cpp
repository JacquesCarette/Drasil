#include <iostream>
#include <string>
#include <vector>

#include "boost/numeric/odeint.hpp"
#include "T_ODE.hpp"

using std::string;
using std::vector;

int main(int argc, const char *argv[]) {
    double c = 3.5;
    
    T_ODE ode = T_ODE(c);
    vector<double> T(0);
    double currVal = 1.0;
    boost::numeric::odeint::integrate_const(boost::numeric::odeint::make_controlled(1.0e-3, 1.0e-3, boost::numeric::odeint::runge_kutta_dopri5<double>()), ode, currVal, 0.0, 10.0, 1.0, Populate_T(T));
    
    std::cout << "[";
    for (int list_i1 = 0; list_i1 < (int)(T.size()) - 1; list_i1++) {
        std::cout << T.at(list_i1);
        std::cout << ", ";
    }
    if ((int)(T.size()) > 0) {
        std::cout << T.at((int)(T.size()) - 1);
    }
    std::cout << "]";
    
    return 0;
}
