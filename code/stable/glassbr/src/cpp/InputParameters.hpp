/** \file InputParameters.hpp
    \author Nikitha Krithnan and W. Spencer Smith
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

/** \brief Structure for holding the input values and derived values
*/
class InputParameters {
    public:
        double a;
        double b;
        double w;
        double P_btol;
        double TNT;
        string g;
        double t;
        double SD_x;
        double SD_y;
        double SD_z;
        double h;
        double LDF;
        double GTF;
        double SD;
        double AR;
        double w_TNT;
        
        ~InputParameters();
    
    private:
};

#endif
