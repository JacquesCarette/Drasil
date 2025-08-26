#include "OutputFormat.hpp"

#include <fstream>
#include <iostream>
#include <string>

using std::ofstream;
using std::string;

void write_output(bool isSafePb, bool isSafeLR, double P_b, double J) {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function write_output called with inputs: {" << std::endl;
    outfile << "  isSafePb = ";
    outfile << isSafePb;
    outfile << ", " << std::endl;
    outfile << "  isSafeLR = ";
    outfile << isSafeLR;
    outfile << ", " << std::endl;
    outfile << "  P_b = ";
    outfile << P_b;
    outfile << ", " << std::endl;
    outfile << "  J = ";
    outfile << J << std::endl;
    outfile << "  }" << std::endl;
    outfile.close();
    
    ofstream outputfile;
    outputfile.open("output.txt", std::fstream::out);
    outputfile << "isSafePb = ";
    outputfile << isSafePb << std::endl;
    outputfile << "isSafeLR = ";
    outputfile << isSafeLR << std::endl;
    outputfile << "P_b = ";
    outputfile << P_b << std::endl;
    outputfile << "J = ";
    outputfile << J << std::endl;
    outputfile.close();
}
