#include "Calculations.hpp"

#include <vector>

#include "ODE.hpp"
#include "Populate.hpp"
#include "boost/numeric/odeint/integrate/integrate_const.hpp"
#include "boost/numeric/odeint/stepper/generation.hpp"
#include "boost/numeric/odeint/stepper/runge_kutta_dopri5.hpp"

using std::vector;

double func_V_W(double V_tank) {
    return V_tank;
}

double func_m_W(double rho_W, double V_W) {
    return V_W * rho_W;
}

double func_tau_W(double C_W, double h_C, double A_C, double m_W) {
    return m_W * C_W / (h_C * A_C);
}

vector<double> func_T_W(double T_C, double T_init, double t_final, double A_tol, double R_tol, double t_step, double tau_W) {
    vector<double> T_W;
    ODE ode = ODE(tau_W, T_C);
    vector<double> currVals{T_init};
    Populate pop = Populate(T_W);
    
    boost::numeric::odeint::runge_kutta_dopri5<vector<double>> rk = boost::numeric::odeint::runge_kutta_dopri5<vector<double>>();
    auto stepper = boost::numeric::odeint::make_controlled(A_tol, R_tol, rk);
    boost::numeric::odeint::integrate_const(stepper, ode, currVals, 0.0, t_final, t_step, pop);
    
    return T_W;
}
