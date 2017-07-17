#ifndef OutputFormat_h
#define OutputFormat_h

#include <string>
#include <vector>

using std::string;
using std::vector;
using std::ifstream;
using std::ofstream;

namespace GlassBR_program {
    
    
    void write_output(string filename, bool is_safe1, bool is_safe2, double P_b);
}

#endif
