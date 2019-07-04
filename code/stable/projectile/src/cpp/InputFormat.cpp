#include "InputFormat.hpp"

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

#include "InputParameters.hpp"

void get_input(string filename, InputParameters &inParams) {
    ifstream infile;
    string line;
    vector<string> lines(0);
    vector<string> linetokens(0);
    infile.open(filename, std::fstream::in);
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> inParams.v_launch;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> inParams.angle;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> inParams.p_target;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.close();
}

