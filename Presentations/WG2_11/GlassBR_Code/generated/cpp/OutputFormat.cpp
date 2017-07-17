#include "OutputFormat.hpp"


#include <algorithm>
#include <iostream>
#include <fstream>
#include <iterator>
#include <string>
#include <math.h>
#include <sstream>
#include <vector>

using namespace GlassBR_program;
using std::string;
using std::vector;
using std::ifstream;
using std::ofstream;

void GlassBR_program::write_output(string filename, bool is_safe1, bool is_safe2, double P_b) {
    ofstream outfile;
    outfile.open(filename, std::fstream::out | std::fstream::app);
    outfile << "is_safe1 = ";
    outfile << is_safe1 << std::endl;
    outfile << "is_safe2 = ";
    outfile << is_safe2 << std::endl;
    outfile << "P_b = ";
    outfile << P_b << std::endl;
    outfile.close();
}

