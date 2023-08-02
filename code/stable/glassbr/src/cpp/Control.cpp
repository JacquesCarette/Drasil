/** \file Control.cpp
    \author Nikitha Krithnan and W. Spencer Smith
    \brief Controls the flow of the program
*/
#include <fstream>
#include <iostream>
#include <string>

#include "Calculations.hpp"
#include "DerivedValues.hpp"
#include "InputConstraints.hpp"
#include "InputFormat.hpp"
#include "InputParameters.hpp"
#include "OutputFormat.hpp"

using std::ofstream;
using std::string;

/** \brief Controls the flow of the program
    \param argc Number of command-line arguments
    \param argv List of command-line arguments
    \return exit code
*/
int main(int argc, const char *argv[]) {
    ofstream outfile;
    string filename = argv[1];
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'filename' assigned ";
    outfile << filename;
    outfile << " in module Control" << std::endl;
    outfile.close();
    InputParameters inParams = InputParameters();
    get_input(filename, inParams);
    derived_values(inParams);
    input_constraints(inParams);
    double h = func_h(inParams);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'h' assigned ";
    outfile << h;
    outfile << " in module Control" << std::endl;
    outfile.close();
    int GTF = func_GTF(inParams);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'GTF' assigned ";
    outfile << GTF;
    outfile << " in module Control" << std::endl;
    outfile.close();
    double AR = func_AR(inParams);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'AR' assigned ";
    outfile << AR;
    outfile << " in module Control" << std::endl;
    outfile.close();
    double q = func_q(inParams);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'q' assigned ";
    outfile << q;
    outfile << " in module Control" << std::endl;
    outfile.close();
    double q_hat = func_q_hat(inParams, q, h, GTF);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'q_hat' assigned ";
    outfile << q_hat;
    outfile << " in module Control" << std::endl;
    outfile.close();
    double J_tol = func_J_tol(inParams, h);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'J_tol' assigned ";
    outfile << J_tol;
    outfile << " in module Control" << std::endl;
    outfile.close();
    double J = func_J(AR, q_hat);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'J' assigned ";
    outfile << J;
    outfile << " in module Control" << std::endl;
    outfile.close();
    double q_hat_tol = func_q_hat_tol(AR, J_tol);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'q_hat_tol' assigned ";
    outfile << q_hat_tol;
    outfile << " in module Control" << std::endl;
    outfile.close();
    double B = func_B(inParams, h, J);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'B' assigned ";
    outfile << B;
    outfile << " in module Control" << std::endl;
    outfile.close();
    double NFL = func_NFL(inParams, q_hat_tol, h);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'NFL' assigned ";
    outfile << NFL;
    outfile << " in module Control" << std::endl;
    outfile.close();
    double P_b = func_P_b(B);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'P_b' assigned ";
    outfile << P_b;
    outfile << " in module Control" << std::endl;
    outfile.close();
    double LR = func_LR(NFL, GTF);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'LR' assigned ";
    outfile << LR;
    outfile << " in module Control" << std::endl;
    outfile.close();
    bool isSafePb = func_isSafePb(inParams, P_b);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'isSafePb' assigned ";
    outfile << isSafePb;
    outfile << " in module Control" << std::endl;
    outfile.close();
    bool isSafeLR = func_isSafeLR(LR, q);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'isSafeLR' assigned ";
    outfile << isSafeLR;
    outfile << " in module Control" << std::endl;
    outfile.close();
    write_output(isSafePb, isSafeLR, P_b, J);
    
    return 0;
}
