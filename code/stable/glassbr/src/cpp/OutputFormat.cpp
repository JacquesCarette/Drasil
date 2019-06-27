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
    ofstream outputfile;
    outputfile.open("output.txt", std::fstream::out);
    outputfile << "is_safePb = ";
    outputfile << is_safePb << std::endl;
    outputfile << "is_safeLR = ";
    outputfile << is_safeLR << std::endl;
    outputfile << "P_b = ";
    outputfile << P_b << std::endl;
    outputfile.close();
}

