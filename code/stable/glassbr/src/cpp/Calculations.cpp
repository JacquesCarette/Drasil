#include "Calculations.hpp"

#include <fstream>
#include <iostream>
#include <math.h>
#include <string>

#include "InputParameters.hpp"
#include "Interpolation.hpp"

using std::ofstream;
using std::string;

double func_h(InputParameters &inParams) {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function func_h called with inputs: {" << std::endl;
    outfile << "  inParams = ";
    outfile << "Instance of InputParameters object" << std::endl;
    outfile << "  }" << std::endl;
    outfile.close();
    
    return 1.0 / 1000.0 * (inParams.t == 2.5 ? 2.16 : inParams.t == 2.7 ? 2.59 : inParams.t == 3.0 ? 2.92 : inParams.t == 4.0 ? 3.78 : inParams.t == 5.0 ? 4.57 : inParams.t == 6.0 ? 5.56 : inParams.t == 8.0 ? 7.42 : inParams.t == 10.0 ? 9.02 : inParams.t == 12.0 ? 11.91 : inParams.t == 16.0 ? 15.09 : inParams.t == 19.0 ? 18.26 : 21.44);
}

int func_GTF(InputParameters &inParams) {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function func_GTF called with inputs: {" << std::endl;
    outfile << "  inParams = ";
    outfile << "Instance of InputParameters object" << std::endl;
    outfile << "  }" << std::endl;
    outfile.close();
    
    if (inParams.g == "AN") {
        return 1;
    }
    else if (inParams.g == "FT") {
        return 4;
    }
    else if (inParams.g == "HS") {
        return 2;
    }
    else {
        throw("Undefined case encountered in function func_GTF");
    }
}

double func_AR(InputParameters &inParams) {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function func_AR called with inputs: {" << std::endl;
    outfile << "  inParams = ";
    outfile << "Instance of InputParameters object" << std::endl;
    outfile << "  }" << std::endl;
    outfile.close();
    
    return inParams.a / inParams.b;
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

double func_q_hat(InputParameters &inParams, double q, double h, int GTF) {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function func_q_hat called with inputs: {" << std::endl;
    outfile << "  inParams = ";
    outfile << "Instance of InputParameters object";
    outfile << ", " << std::endl;
    outfile << "  q = ";
    outfile << q;
    outfile << ", " << std::endl;
    outfile << "  h = ";
    outfile << h;
    outfile << ", " << std::endl;
    outfile << "  GTF = ";
    outfile << GTF << std::endl;
    outfile << "  }" << std::endl;
    outfile.close();
    
    return q * pow(inParams.a * inParams.b, 2.0) / (7.17e10 * pow(h, 4.0) * GTF);
}

double func_J_tol(InputParameters &inParams, double h) {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function func_J_tol called with inputs: {" << std::endl;
    outfile << "  inParams = ";
    outfile << "Instance of InputParameters object";
    outfile << ", " << std::endl;
    outfile << "  h = ";
    outfile << h << std::endl;
    outfile << "  }" << std::endl;
    outfile.close();
    
    return log(log(1.0 / (1.0 - inParams.P_btol)) * (pow(inParams.a * inParams.b, 7.0 - 1.0) / (2.86e-53 * pow(7.17e10 * pow(h, 2.0), 7.0) * inParams.LDF)));
}

double func_J(double AR, double q_hat) {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function func_J called with inputs: {" << std::endl;
    outfile << "  AR = ";
    outfile << AR;
    outfile << ", " << std::endl;
    outfile << "  q_hat = ";
    outfile << q_hat << std::endl;
    outfile << "  }" << std::endl;
    outfile.close();
    
    return interpZ("SDF.txt", AR, q_hat);
}

double func_q_hat_tol(double AR, double J_tol) {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function func_q_hat_tol called with inputs: {" << std::endl;
    outfile << "  AR = ";
    outfile << AR;
    outfile << ", " << std::endl;
    outfile << "  J_tol = ";
    outfile << J_tol << std::endl;
    outfile << "  }" << std::endl;
    outfile.close();
    
    return interpY("SDF.txt", AR, J_tol);
}

double func_B(InputParameters &inParams, double h, double J) {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function func_B called with inputs: {" << std::endl;
    outfile << "  inParams = ";
    outfile << "Instance of InputParameters object";
    outfile << ", " << std::endl;
    outfile << "  h = ";
    outfile << h;
    outfile << ", " << std::endl;
    outfile << "  J = ";
    outfile << J << std::endl;
    outfile << "  }" << std::endl;
    outfile.close();
    
    return 2.86e-53 / pow(inParams.a * inParams.b, 7.0 - 1.0) * pow(7.17e10 * pow(h, 2.0), 7.0) * inParams.LDF * exp(J);
}

double func_NFL(InputParameters &inParams, double q_hat_tol, double h) {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function func_NFL called with inputs: {" << std::endl;
    outfile << "  inParams = ";
    outfile << "Instance of InputParameters object";
    outfile << ", " << std::endl;
    outfile << "  q_hat_tol = ";
    outfile << q_hat_tol;
    outfile << ", " << std::endl;
    outfile << "  h = ";
    outfile << h << std::endl;
    outfile << "  }" << std::endl;
    outfile.close();
    
    return q_hat_tol * 7.17e10 * pow(h, 4.0) / pow(inParams.a * inParams.b, 2.0);
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

double func_LR(double NFL, int GTF) {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function func_LR called with inputs: {" << std::endl;
    outfile << "  NFL = ";
    outfile << NFL;
    outfile << ", " << std::endl;
    outfile << "  GTF = ";
    outfile << GTF << std::endl;
    outfile << "  }" << std::endl;
    outfile.close();
    
    return NFL * GTF * 1.0;
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
