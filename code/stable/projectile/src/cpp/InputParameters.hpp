#ifndef InputParameters_h
#define InputParameters_h

#include <string>
#include <vector>

using std::string;
using std::vector;
using std::ifstream;
using std::ofstream;

class InputParameters {
    public:
        double v_launch;
        double angle;
        double p_target;
        
        ~InputParameters();
    
    private:
};

#endif
