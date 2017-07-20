#ifndef InputParameters_h
#define InputParameters_h

#include <string>
#include <vector>

using std::string;
using std::vector;
using std::ifstream;
using std::ofstream;

namespace GlassBR {
    class InputParameters;
    
    class InputParameters {
        public:
            double a;
            double b;
            double t;
            int gt;
            double w;
            double tnt;
            double sdx;
            double sdy;
            double sdz;
            double pbtol;
            double asprat;
            double sd;
            double h;
            double gtf;
            double ldf;
            double wtnt;
            double E;
            double td;
            double m;
            double k;
            double lsf;
            
            InputParameters();
            ~InputParameters();
        
        private:
    };
    
}

#endif
