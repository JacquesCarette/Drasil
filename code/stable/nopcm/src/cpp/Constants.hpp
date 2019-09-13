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
        static const double L_min;
        static const double L_max;
        static const double rho_W_min;
        static const double rho_W_max;
        static const double A_C_max;
        static const double C_W_min;
        static const double C_W_max;
        static const double h_C_min;
        static const double h_C_max;
        static const double t_final_max;
        static const double AR_min;
        static const double AR_max;
        
        ~Constants();
    
    private:
};

#endif
