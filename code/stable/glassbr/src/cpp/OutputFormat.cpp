#include "OutputFormat.hpp"

#define _USE_MATH_DEFINES
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
    outfile.open("log.txt", std::fstream::app);
    outfile << "function write_output called with inputs: {" << std::endl;
    outfile << "  is_safePb = ";
    outfile << is_safePb;
    outfile << ", " << std::endl;
    outfile << "  is_safeLR = ";
    outfile << is_safeLR;
    outfile << ", " << std::endl;
    outfile << "  P_b = ";
    outfile << P_b << std::endl;
    outfile << "  }" << std::endl;
    outfile.close();
    
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
