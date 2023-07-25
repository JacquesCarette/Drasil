/** \file ODE.hpp
    \author Dong Chen
    \brief Class representing an ODE system
*/
#ifndef ODE_h
#define ODE_h

#include <vector>

using std::vector;

/** \brief Class representing an ODE system
*/
class ODE {
    public:
        /** \brief Constructor for ODE objects
            \param m_1 mass of the first object (kg)
            \param m_2 mass of the second object (kg)
            \param L_1 length of the first rod (m)
            \param L_2 length of the second rod (m)
        */
        ODE(double m_1, double m_2, double L_1, double L_2);
        /** \brief function representation of ODE system
            \param theta dependent variables (rad)
            \param dtheta change in dependent variables (rad)
            \param t current independent variable value in ODE solution
        */
        void operator()(vector<double> theta, vector<double> &dtheta, double t);
    
    private:
        double m_1;
        double m_2;
        double L_1;
        double L_2;
        
};

#endif
