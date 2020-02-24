#include "InputFormat.hpp"

#include <algorithm>
#include <fstream>
#include <limits>
#include <string>

using std::ifstream;
using std::string;

void get_input(string filename, double &v_launch, double &theta, double &p_target) {
    ifstream infile;
    infile.open(filename, std::fstream::in);
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> v_launch;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> theta;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> p_target;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.close();
}
