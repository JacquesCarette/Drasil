#ifndef Calculations_h
#define Calculations_h

#include <string>
#include <vector>

using std::string;
using std::vector;
using std::ifstream;
using std::ofstream;

namespace GlassBR_program {
    
    
    double calc_B(double k, double a, double b, double m, double E, double h, double LDF, double J);
    
    double calc_h(double t);
    
    double calc_LDF(double t_d, double m);
    
    double calc_J(double J, double q_hat, double a, double b);
    
    double calc_NFL(double q_hat_tol, double E, double h, double a, double b);
    
    double calc_GTF(double g);
    
    double calc_q_hat(double q, double a, double b, double E, double h, double GTF);
    
    double calc_q_hat_tol(double q_hat_tol, double J_tol, double a, double b);
    
    double calc_J_tol(double P_btol, double a, double b, double m, double k, double E, double h, double LDF);
}

#endif
