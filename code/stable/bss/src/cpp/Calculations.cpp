#include "Calculations.hpp"

#include <vector>

#include "ODE.hpp"
#include "Populate.hpp"
#include "boost/numeric/odeint/integrate/integrate_const.hpp"
#include "boost/numeric/odeint/stepper/generation.hpp"
#include "boost/numeric/odeint/stepper/runge_kutta_dopri5.hpp"

using std::vector;

vector<vector<double>> func_q(double m_2, double m_1, double x_1_0, double y_1_0, double x_2_0, double y_2_0, double v_x1_0, double v_y1_0, double v_x2_0, double v_y2_0, double t_final) {
    vector<vector<double>> q;
    ODE ode = ODE(m_1, m_2, x_1_0, y_1_0, x_2_0, y_2_0, v_x1_0, v_y1_0, v_x2_0, v_y2_0, t_final);
    vector<double> currVals{x_1_0, y_1_0, x_2_0, y_2_0, v_x1_0, v_y1_0, v_x2_0, v_y2_0};
    Populate pop = Populate(q);
    
    boost::numeric::odeint::runge_kutta_dopri5<vector<double>> rk = boost::numeric::odeint::runge_kutta_dopri5<vector<double>>();
    auto stepper = boost::numeric::odeint::make_controlled(1.0e-8, 1.0e-8, rk);
    boost::numeric::odeint::integrate_const(stepper, ode, currVals, 0.0, t_final, 10.0, pop);
    
    return q;
}
