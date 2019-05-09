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
    ofstream outfile;
    outfile.open("output.txt", std::fstream::out | std::fstream::app);
    outfile << "T_W = ";
    outfile << inParams.T_W << std::endl;
    outfile << "E_W = ";
    outfile << inParams.E_W << std::endl;
    outfile.close();
}

