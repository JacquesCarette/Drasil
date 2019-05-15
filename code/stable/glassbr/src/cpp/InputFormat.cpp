#include "InputFormat.hpp"

#include "InputParameters.hpp"

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

void func_get_input(string filename, InputParameters &inParams) {
    ifstream infile;
    string line;
    vector<string> lines(0);
    vector<string> linetokens(0);
    infile.open(filename, std::fstream::in);
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> inParams.a;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> inParams.b;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> inParams.t;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> inParams.g;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> inParams.w;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> inParams.TNT;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> inParams.SD_x;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> inParams.SD_y;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> inParams.SD_z;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> inParams.P_btol;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.close();
}

