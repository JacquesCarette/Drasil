#include "InputFormat.hpp"

#include "InputParameters.hpp"

#include <algorithm>
#include <iostream>
#include <fstream>
#include <iterator>
#include <string>
#include <math.h>
#include <sstream>
#include <vector>

using namespace GlassBR;
using std::string;
using std::vector;
using std::ifstream;
using std::ofstream;

void GlassBR::get_input(string filename, InputParameters &inparams) {
    ifstream infile;
    infile.open(filename, std::fstream::in);
    infile >> inparams.a;
    infile >> inparams.b;
    infile >> inparams.t;
    infile >> inparams.gt;
    infile >> inparams.w;
    infile >> inparams.tnt;
    infile >> inparams.sdx;
    infile >> inparams.sdy;
    infile >> inparams.sdz;
    infile >> inparams.pbtol;
    infile.close();
}

