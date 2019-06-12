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

#include "InputConstraints.hpp"
#include "InputParameters.hpp"
#include "InputFormat.hpp"
#include "Calculations.hpp"
#include "OutputFormat.hpp"

int main(int argc, const char *argv[]) {
    string inputfile = argv[1];
    InputParameters inParams = InputParameters();
    func_get_input(inputfile, inParams);
    input_constraints(inParams);
    write_output(inParams);
    return 0;
}

