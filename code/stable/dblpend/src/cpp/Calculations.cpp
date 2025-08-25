#include "Calculations.hpp"

#include <vector>

#include "ODE.hpp"
#include "Populate.hpp"
#include "boost/numeric/odeint/integrate/integrate_const.hpp"
#include "boost/numeric/odeint/stepper/generation.hpp"
#include "boost/numeric/odeint/stepper/runge_kutta_dopri5.hpp"

using std::vector;

vector<double> func_theta(double m_1, double m_2, double L_2, double L_1) {
    vector<double> theta;
    ODE ode = ODE(m_1, m_2, L_1, L_2);
    vector<double> currVals{1.3463968515384828, 0.0, 2.356194490192345, 0.0};
    Populate pop = Populate(theta);
    
    boost::numeric::odeint::runge_kutta_dopri5<vector<double>> rk = boost::numeric::odeint::runge_kutta_dopri5<vector<double>>();
    auto stepper = boost::numeric::odeint::make_controlled(1.0e-6, 1.0e-6, rk);
    boost::numeric::odeint::integrate_const(stepper, ode, currVals, 0.0, 20.0, 1.0e-3, pop);
    
    return theta;
}
