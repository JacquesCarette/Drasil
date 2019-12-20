#include <iostream>
#include <string>
#include <fstream>

#include "OutputFormat.hpp"

using std::string;
using std::ofstream;

void write_output(double T_W, double E_W) {
    ofstream outputfile;
    outputfile.open("output.txt", std::fstream::out);
    outputfile << "T_W = ";
    outputfile << T_W << std::endl;
    outputfile << "E_W = ";
    outputfile << E_W << std::endl;
    outputfile.close();
}
