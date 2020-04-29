#ifndef T_ODE_h
#define T_ODE_h

#include <vector>

using std::vector;

class T_ODE {
    public:
        vector<double> T;
        
        T_ODE(double c);
        void operator()(double T, float &dTdt, double t);
    
    private:
        double c;
        
};

class Populate_T {
    public:
        vector<double> &T;
        
        Populate_T(vector<double> &T);
        void operator()(double &T, double t);
    
    private:
};

#endif
