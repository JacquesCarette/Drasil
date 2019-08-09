/** \file Control.cpp
    \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
    \brief Controls the flow of the program
*/
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
#include "InputFormat.hpp"
#include "InputConstraints.hpp"
#include "OutputFormat.hpp"
#include "Calculations.hpp"

/** \brief Controls the flow of the program
    \param argc Number of command-line arguments
    \param argv List of command-line arguments
*/
int main(int argc, const char *argv[]) {
    string filename = argv[1];
    InputParameters inParams = InputParameters();
    get_input(filename, inParams);
    input_constraints(inParams);
    double t_flight = func_t_flight(inParams);
    double p_land = func_p_land(inParams);
    double d_offset = func_d_offset(inParams, p_land);
    string s = func_s(inParams, d_offset);
    write_output(s, d_offset);
    
    return 0;
}

