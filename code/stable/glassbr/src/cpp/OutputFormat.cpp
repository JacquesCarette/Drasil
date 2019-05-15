#include "OutputFormat.hpp"


#include <algorithm>
#include <iostream>
#include <fstream>
#include <iterator>
#include <string>
#include <math.h>
#include <sstream>
#include <limits>
#include <vector>

using std::string;
using std::vector;
using std::ifstream;
using std::ofstream;

void write_output(bool is_safePb, bool is_safeLR, double P_b) {
    ofstream outfile;
    outfile.open("output.txt", std::fstream::out | std::fstream::app);
    outfile << "is_safePb = ";
    outfile << is_safePb << std::endl;
    outfile << "is_safeLR = ";
    outfile << is_safeLR << std::endl;
    outfile << "P_b = ";
    outfile << P_b << std::endl;
    outfile.close();
}

