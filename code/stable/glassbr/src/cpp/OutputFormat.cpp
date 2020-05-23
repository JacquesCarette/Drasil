#include "OutputFormat.hpp"

#include <fstream>
#include <iostream>
#include <string>

using std::ofstream;
using std::string;

void write_output(bool is_safePb, bool is_safeLR, double P_b, double J) {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function write_output called with inputs: {" << std::endl;
    outfile << "  is_safePb = ";
    outfile << is_safePb;
    outfile << ", " << std::endl;
    outfile << "  is_safeLR = ";
    outfile << is_safeLR;
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
    outputfile << "is_safePb = ";
    outputfile << is_safePb << std::endl;
    outputfile << "is_safeLR = ";
    outputfile << is_safeLR << std::endl;
    outputfile << "P_b = ";
    outputfile << P_b << std::endl;
    outputfile << "J = ";
    outputfile << J << std::endl;
    outputfile.close();
}
