/** \file Control.cpp
    \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
    \brief Controls the flow of the program
*/
#include <string>

#include "Calculations.hpp"
#include "InputConstraints.hpp"
#include "InputFormat.hpp"
#include "OutputFormat.hpp"

using std::string;

/** \brief Controls the flow of the program
    \param argc Number of command-line arguments
    \param argv List of command-line arguments
    \return exit code
*/
int main(int argc, const char *argv[]) {
    string filename = argv[1];
    double v_launch;
    double theta;
    double p_target;
    double g_vect = 9.8;
    double epsilon = 2.0e-2;
    get_input(filename, v_launch, theta, p_target);
    input_constraints(v_launch, theta, p_target);
    double t_flight = func_t_flight(v_launch, theta, g_vect);
    double p_land = func_p_land(v_launch, theta, g_vect);
    double d_offset = func_d_offset(p_target, p_land);
    string s = func_s(p_target, epsilon, d_offset);
    write_output(s, d_offset);
    
    return 0;
}
