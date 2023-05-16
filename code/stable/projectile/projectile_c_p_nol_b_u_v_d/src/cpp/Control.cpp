/** \file Control.cpp
    \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
    \brief Controls the flow of the program
*/
#include <string>

#include "Calculations.hpp"
#include "InputParameters.hpp"
#include "OutputFormat.hpp"

using std::string;

/** \brief Controls the flow of the program
    \param argc Number of command-line arguments
    \param argv List of command-line arguments
    \return exit code
*/
int main(int argc, const char *argv[]) {
    string filename = argv[1];
    double g = 9.8;
    double epsilon = 2.0e-2;
    InputParameters inParams = InputParameters(filename);
    double t_flight = func_t_flight(inParams, g);
    double p_land = func_p_land(inParams, g);
    double d_offset = func_d_offset(inParams, p_land);
    string s = func_s(inParams, epsilon, d_offset);
    write_output(s, d_offset, t_flight);
    
    return 0;
}
