/** \file Populate.hpp
    \author Thulasi Jegatheesan
    \brief Class for populating a list during an ODE solution process
*/
#ifndef Populate_h
#define Populate_h

#include <vector>

using std::vector;

/** \brief Class for populating a list during an ODE solution process
*/
class Populate {
    public:
        /** \brief Constructor for Populate objects
            \param T_W temperature of the water: the average kinetic energy of the particles within the water (degreeC)
        */
        Populate(vector<double> &T_W);
        /** \brief appends solution point for current ODE solution step
            \param y current dependent variable value in ODE solution
            \param t current independent variable value in ODE solution
        */
        void operator()(vector<double> &y, double t);
    
    private:
        vector<double> &T_W;
        
};

#endif
