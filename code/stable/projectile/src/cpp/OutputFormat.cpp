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

void write_output(string s, double d_offset) {
    ofstream outputfile;
    outputfile.open("output.txt", std::fstream::out);
    outputfile << "s = ";
    outputfile << s << std::endl;
    outputfile << "d_offset = ";
    outputfile << d_offset << std::endl;
    outputfile.close();
}

