/** \file Populate.hpp
    \author Naveen Ganesh Muralidharan
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
            \param y_t Process Variable: The output value from the power plant
        */
        Populate(vector<double> &y_t);
        /** \brief appends solution point for current ODE solution step
            \param y current dependent variable value in ODE solution
            \param t current independent variable value in ODE solution
        */
        void operator()(vector<double> &y, double t);
    
    private:
        vector<double> &y_t;
        
};

#endif
