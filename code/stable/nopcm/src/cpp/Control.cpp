
#include "InputParameters.hpp"
#include "DerivedValues.hpp"
#include "InputConstraints.hpp"
#include "InputFormat.hpp"
#include "OutputFormat.hpp"
#include "Calculations.hpp"

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

int main(int argc, const char *argv[]) {
    string inputfile = argv[1];
    InputParameters inParams = InputParameters();
    func_get_input(inputfile, inParams);
    derived_values(inParams);
    input_constraints(inParams);
    write_output(inParams);
    
    return 0;
}

