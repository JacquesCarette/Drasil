/** \file Control.cpp
    \author Nikitha Krithnan and W. Spencer Smith
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
#include "DerivedValues.hpp"
#include "InputConstraints.hpp"
#include "OutputFormat.hpp"
#include "Calculations.hpp"

/** \brief Controls the flow of the program
    \param argc Number of command-line arguments
    \param argv List of command-line arguments
    \return exit code
*/
int main(int argc, const char *argv[]) {
    ofstream outfile;
    string filename = argv[1];
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'filename' assigned to ";
    outfile << filename;
    outfile << " in module Control" << std::endl;
    outfile.close();
    InputParameters inParams = InputParameters();
    get_input(filename, inParams);
    derived_values(inParams);
    input_constraints(inParams);
    double J_tol = func_J_tol(inParams);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'J_tol' assigned to ";
    outfile << J_tol;
    outfile << " in module Control" << std::endl;
    outfile.close();
    double q = func_q(inParams);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'q' assigned to ";
    outfile << q;
    outfile << " in module Control" << std::endl;
    outfile.close();
    double q_hat = func_q_hat(inParams, q);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'q_hat' assigned to ";
    outfile << q_hat;
    outfile << " in module Control" << std::endl;
    outfile.close();
    double q_hat_tol = func_q_hat_tol(inParams, J_tol);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'q_hat_tol' assigned to ";
    outfile << q_hat_tol;
    outfile << " in module Control" << std::endl;
    outfile.close();
    double J = func_J(inParams, q_hat);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'J' assigned to ";
    outfile << J;
    outfile << " in module Control" << std::endl;
    outfile.close();
    double NFL = func_NFL(inParams, q_hat_tol);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'NFL' assigned to ";
    outfile << NFL;
    outfile << " in module Control" << std::endl;
    outfile.close();
    double B = func_B(inParams, J);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'B' assigned to ";
    outfile << B;
    outfile << " in module Control" << std::endl;
    outfile.close();
    double LR = func_LR(inParams, NFL);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'LR' assigned to ";
    outfile << LR;
    outfile << " in module Control" << std::endl;
    outfile.close();
    bool is_safeLR = func_is_safeLR(LR, q);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'is_safeLR' assigned to ";
    outfile << is_safeLR;
    outfile << " in module Control" << std::endl;
    outfile.close();
    double P_b = func_P_b(B);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'P_b' assigned to ";
    outfile << P_b;
    outfile << " in module Control" << std::endl;
    outfile.close();
    bool is_safePb = func_is_safePb(inParams, P_b);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'is_safePb' assigned to ";
    outfile << is_safePb;
    outfile << " in module Control" << std::endl;
    outfile.close();
    write_output(is_safePb, is_safeLR, P_b);
    
    return 0;
}

