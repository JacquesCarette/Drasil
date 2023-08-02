/** \file InputParameters.hpp
    \author Nikitha Krithnan and W. Spencer Smith
    \brief Provides the structure for holding input values
*/
#ifndef InputParameters_h
#define InputParameters_h

#include <string>

using std::string;

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
        double SD;
        double w_TNT;
        
};

#endif
