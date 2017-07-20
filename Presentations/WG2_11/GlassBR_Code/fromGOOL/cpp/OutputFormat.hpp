#include "InputParameters.hpp"
#ifndef OutputFormat_h
#define OutputFormat_h

#include <string>
#include <vector>

using std::string;
using std::vector;
using std::ifstream;
using std::ofstream;

namespace GlassBR {
    
    
    void display_output(string filename, double q, double j, double q_hat_tol, double pb, double lr, double nfl, bool is_safe1, bool is_safe2, InputParameters &inparams);
}

#endif
