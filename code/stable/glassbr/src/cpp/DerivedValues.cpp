#include "DerivedValues.hpp"

#include <fstream>
#include <iostream>
#include <math.h>
#include <string>

#include "InputParameters.hpp"

using std::ofstream;
using std::string;

void derived_values(InputParameters &inParams) {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function derived_values called with inputs: {" << std::endl;
    outfile << "  inParams = ";
    outfile << "Instance of InputParameters object" << std::endl;
    outfile << "  }" << std::endl;
    outfile.close();
    
    inParams.LDF = pow(3.0 / 60.0, 7.0 / 16.0);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'inParams.LDF' assigned ";
    outfile << inParams.LDF;
    outfile << " in module DerivedValues" << std::endl;
    outfile.close();
    
    inParams.SD = sqrt(pow(inParams.SD_x, 2.0) + pow(inParams.SD_y, 2.0) + pow(inParams.SD_z, 2.0));
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'inParams.SD' assigned ";
    outfile << inParams.SD;
    outfile << " in module DerivedValues" << std::endl;
    outfile.close();
    
    inParams.w_TNT = inParams.w * inParams.TNT;
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'inParams.w_TNT' assigned ";
    outfile << inParams.w_TNT;
    outfile << " in module DerivedValues" << std::endl;
    outfile.close();
}
