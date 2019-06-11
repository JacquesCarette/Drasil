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

void write_output(InputParameters &inParams) {
    ofstream outputfile;
    outputfile.open("output.txt", std::fstream::out);
    outputfile << "T_W = ";
    outputfile << inParams.T_W << std::endl;
    outputfile << "E_W = ";
    outputfile << inParams.E_W << std::endl;
    outputfile.close();
}

