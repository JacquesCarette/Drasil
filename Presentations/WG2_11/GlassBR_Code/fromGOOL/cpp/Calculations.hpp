#include "InputParameters.hpp"
#ifndef Calculations_h
#define Calculations_h

#include <string>
#include <vector>

using std::string;
using std::vector;
using std::ifstream;
using std::ofstream;

namespace GlassBR {
    
    
    double calc_q_hat(double q, InputParameters &inparams);
    
    double calc_j_tol(InputParameters &inparams);
    
    double calc_pb(double j, InputParameters &inparams);
    
    double calc_nfl(double q_hat_tol, InputParameters &inparams);
    
    double calc_lr(double nfl, InputParameters &inparams);
    
    bool calc_is_safe1(double pb, InputParameters &inparams);
    
    bool calc_is_safe2(double lr, double q);
}

#endif
