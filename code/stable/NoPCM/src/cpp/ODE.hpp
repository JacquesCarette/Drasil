/** \file ODE.hpp
    \author Thulasi Jegatheesan
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
            \param tau_W ODE parameter for water related to decay time: derived parameter based on rate of change of temperature of water (s)
            \param T_C temperature of the heating coil: the average kinetic energy of the particles within the coil (degreeC)
        */
        ODE(double tau_W, double T_C);
        /** \brief function representation of ODE system
            \param T_W temperature of the water: the average kinetic energy of the particles within the water (degreeC)
            \param dT_W change in temperature of the water (degreeC)
            \param t current independent variable value in ODE solution
        */
        void operator()(vector<double> T_W, vector<double> &dT_W, double t);
    
    private:
        double tau_W;
        double T_C;
        
};

#endif
