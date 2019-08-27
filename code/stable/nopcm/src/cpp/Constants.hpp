/** \file Constants.hpp
    \author Thulasi Jegatheesan
    \brief Provides the structure for holding constant values
*/
#ifndef Constants_h
#define Constants_h

#include <string>
#include <vector>

using std::string;
using std::vector;
using std::ifstream;
using std::ofstream;

/** \brief Structure for holding the constant values
*/
class Constants {
    public:
        double L_min;
        double L_max;
        double rho_W_min;
        double rho_W_max;
        double A_C_max;
        double C_W_min;
        double C_W_max;
        double h_C_min;
        double h_C_max;
        double t_final_max;
        double AR_min;
        double AR_max;
        
        /** \brief Assigns values to variables for constants
        */
        Constants();
        ~Constants();
    
    private:
};

#endif
