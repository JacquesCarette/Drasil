#ifndef InputParameters_h
#define InputParameters_h

#include <string>
#include <vector>

using std::string;
using std::vector;
using std::ifstream;
using std::ofstream;

class InputParameters;

class InputParameters {
    public:
        double a;
        double b;
        double SD;
        double w;
        double AR;
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
        double w_TNT;
        
        InputParameters();
        ~InputParameters();
    
    private:
};


#endif
