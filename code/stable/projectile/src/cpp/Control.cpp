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
    \return exit code
*/
int main(int argc, const char *argv[]) {
    string filename = argv[1];
    InputParameters inParams = InputParameters();
    double g_vect = 9.8;
    double pi = 3.14159265;
    double epsilon = 2.0e-2;
    get_input(inParams, filename);
    input_constraints(inParams, pi);
    double t_flight = func_t_flight(inParams, g_vect);
    double p_land = func_p_land(inParams, g_vect);
    double d_offset = func_d_offset(inParams, p_land);
    string s = func_s(inParams, epsilon, d_offset);
    write_output(s, d_offset);
    
    return 0;
}
