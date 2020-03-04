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
    float v_launch;
    float theta;
    float p_target;
    float g_vect = 9.8f;
    float epsilon = 2.0e-2f;
    get_input(filename, v_launch, theta, p_target);
    input_constraints(v_launch, theta, p_target);
    float t_flight = func_t_flight(v_launch, theta, g_vect);
    float p_land = func_p_land(v_launch, theta, g_vect);
    float d_offset = func_d_offset(p_target, p_land);
    string s = func_s(p_target, epsilon, d_offset);
    write_output(s, d_offset);
    
    return 0;
}
