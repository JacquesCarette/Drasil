/** \file InputParameters.hpp
    \brief Provides the structure for holding input values
*/
#ifndef InputParameters_h
#define InputParameters_h

#include <string>
#include <vector>

using std::string;
using std::vector;
using std::ifstream;
using std::ofstream;

/** \brief Structure for holding the input values
*/
class InputParameters {
    public:
        double v_launch;
        double theta;
        double p_target;
        
        ~InputParameters();
    
    private:
};

#endif
