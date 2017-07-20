#ifndef ReadTable_h
#define ReadTable_h

#include <string>
#include <vector>

using std::string;
using std::vector;
using std::ifstream;
using std::ofstream;

namespace GlassBR {
    
    
    vector<double> read_z_array(string filename);
    
    vector< vector<double> > read_x_array(string filename);
    
    vector< vector<double> > read_y_array(string filename);
}

#endif
