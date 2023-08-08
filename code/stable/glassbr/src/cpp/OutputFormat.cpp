#include "OutputFormat.hpp"

#include <fstream>
#include <iostream>
#include <string>

using std::ofstream;
using std::string;

void write_output(double B, double J, double NFL, double q_hat, double q_hat_tol, double J_tol, bool isSafePb, bool isSafeLR) {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function write_output called with inputs: {" << std::endl;
    outfile << "  B = ";
    outfile << B;
    outfile << ", " << std::endl;
    outfile << "  J = ";
    outfile << J;
    outfile << ", " << std::endl;
    outfile << "  NFL = ";
    outfile << NFL;
    outfile << ", " << std::endl;
    outfile << "  q_hat = ";
    outfile << q_hat;
    outfile << ", " << std::endl;
    outfile << "  q_hat_tol = ";
    outfile << q_hat_tol;
    outfile << ", " << std::endl;
    outfile << "  J_tol = ";
    outfile << J_tol;
    outfile << ", " << std::endl;
    outfile << "  isSafePb = ";
    outfile << isSafePb;
    outfile << ", " << std::endl;
    outfile << "  isSafeLR = ";
    outfile << isSafeLR << std::endl;
    outfile << "  }" << std::endl;
    outfile.close();
    
    ofstream outputfile;
    outputfile.open("output.txt", std::fstream::out);
    outputfile << "B = ";
    outputfile << B << std::endl;
    outputfile << "J = ";
    outputfile << J << std::endl;
    outputfile << "NFL = ";
    outputfile << NFL << std::endl;
    outputfile << "q_hat = ";
    outputfile << q_hat << std::endl;
    outputfile << "q_hat_tol = ";
    outputfile << q_hat_tol << std::endl;
    outputfile << "J_tol = ";
    outputfile << J_tol << std::endl;
    outputfile << "isSafePb = ";
    outputfile << isSafePb << std::endl;
    outputfile << "isSafeLR = ";
    outputfile << isSafeLR << std::endl;
    outputfile.close();
}
