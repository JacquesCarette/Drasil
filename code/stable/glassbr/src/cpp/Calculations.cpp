#include "Calculations.hpp"

#include <fstream>
#include <iostream>
#include <math.h>
#include <string>

#include "InputParameters.hpp"
#include "Interpolation.hpp"

using std::ofstream;
using std::string;

double func_J_tol(InputParameters &inParams) {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function func_J_tol called with inputs: {" << std::endl;
    outfile << "  inParams = ";
    outfile << "Instance of InputParameters object" << std::endl;
    outfile << "  }" << std::endl;
    outfile.close();
    
    return log(log(1.0 / (1.0 - inParams.P_btol)) * (pow(inParams.a * inParams.b, 7.0 - 1.0) / (2.86e-53 * pow(7.17e10 * pow(inParams.h, 2.0), 7.0) * inParams.LDF)));
}

double func_q(InputParameters &inParams) {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function func_q called with inputs: {" << std::endl;
    outfile << "  inParams = ";
    outfile << "Instance of InputParameters object" << std::endl;
    outfile << "  }" << std::endl;
    outfile.close();
    
    return interpY("TSD.txt", inParams.SD, inParams.w_TNT);
}

double func_q_hat(InputParameters &inParams, double q) {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function func_q_hat called with inputs: {" << std::endl;
    outfile << "  inParams = ";
    outfile << "Instance of InputParameters object";
    outfile << ", " << std::endl;
    outfile << "  q = ";
    outfile << q << std::endl;
    outfile << "  }" << std::endl;
    outfile.close();
    
    return q * pow(inParams.a * inParams.b, 2.0) / (7.17e10 * pow(inParams.h, 4.0) * inParams.GTF);
}

double func_q_hat_tol(InputParameters &inParams, double J_tol) {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function func_q_hat_tol called with inputs: {" << std::endl;
    outfile << "  inParams = ";
    outfile << "Instance of InputParameters object";
    outfile << ", " << std::endl;
    outfile << "  J_tol = ";
    outfile << J_tol << std::endl;
    outfile << "  }" << std::endl;
    outfile.close();
    
    return interpY("SDF.txt", inParams.AR, J_tol);
}

double func_J(InputParameters &inParams, double q_hat) {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function func_J called with inputs: {" << std::endl;
    outfile << "  inParams = ";
    outfile << "Instance of InputParameters object";
    outfile << ", " << std::endl;
    outfile << "  q_hat = ";
    outfile << q_hat << std::endl;
    outfile << "  }" << std::endl;
    outfile.close();
    
    return interpZ("SDF.txt", inParams.AR, q_hat);
}

double func_NFL(InputParameters &inParams, double q_hat_tol) {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function func_NFL called with inputs: {" << std::endl;
    outfile << "  inParams = ";
    outfile << "Instance of InputParameters object";
    outfile << ", " << std::endl;
    outfile << "  q_hat_tol = ";
    outfile << q_hat_tol << std::endl;
    outfile << "  }" << std::endl;
    outfile.close();
    
    return q_hat_tol * 7.17e10 * pow(inParams.h, 4.0) / pow(inParams.a * inParams.b, 2.0);
}

double func_B(InputParameters &inParams, double J) {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function func_B called with inputs: {" << std::endl;
    outfile << "  inParams = ";
    outfile << "Instance of InputParameters object";
    outfile << ", " << std::endl;
    outfile << "  J = ";
    outfile << J << std::endl;
    outfile << "  }" << std::endl;
    outfile.close();
    
    return 2.86e-53 / pow(inParams.a * inParams.b, 7.0 - 1.0) * pow(7.17e10 * pow(inParams.h, 2.0), 7.0) * inParams.LDF * exp(J);
}

double func_LR(InputParameters &inParams, double NFL) {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function func_LR called with inputs: {" << std::endl;
    outfile << "  inParams = ";
    outfile << "Instance of InputParameters object";
    outfile << ", " << std::endl;
    outfile << "  NFL = ";
    outfile << NFL << std::endl;
    outfile << "  }" << std::endl;
    outfile.close();
    
    return NFL * inParams.GTF * 1.0;
}

double func_P_b(double B) {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function func_P_b called with inputs: {" << std::endl;
    outfile << "  B = ";
    outfile << B << std::endl;
    outfile << "  }" << std::endl;
    outfile.close();
    
    return 1.0 - exp(-B);
}

bool func_isSafeLR(double LR, double q) {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function func_isSafeLR called with inputs: {" << std::endl;
    outfile << "  LR = ";
    outfile << LR;
    outfile << ", " << std::endl;
    outfile << "  q = ";
    outfile << q << std::endl;
    outfile << "  }" << std::endl;
    outfile.close();
    
    return LR > q;
}

bool func_isSafePb(InputParameters &inParams, double P_b) {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function func_isSafePb called with inputs: {" << std::endl;
    outfile << "  inParams = ";
    outfile << "Instance of InputParameters object";
    outfile << ", " << std::endl;
    outfile << "  P_b = ";
    outfile << P_b << std::endl;
    outfile << "  }" << std::endl;
    outfile.close();
    
    return P_b < inParams.P_btol;
}
