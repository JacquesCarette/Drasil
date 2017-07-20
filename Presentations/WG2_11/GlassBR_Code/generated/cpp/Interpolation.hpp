#ifndef Interpolation_h
#define Interpolation_h

#include <string>
#include <vector>

using std::string;
using std::vector;
using std::ifstream;
using std::ofstream;

namespace GlassBR_program {
    
    
    double lin_interp(double x1, double y1, double x2, double y2, double x);
    
    int indInSeq(vector<double> &arr, double v);
    
    vector<double> matrixCol(vector< vector<double> > &mat, int c);
    
    double interpY(vector< vector<double> > &x_array, vector< vector<double> > &y_array, vector<double> &z_array, double x, double z);
    
    double interpZ(vector< vector<double> > &x_array, vector< vector<double> > &y_array, vector<double> &z_array, double x, double y);
}

#endif
