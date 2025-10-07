#include "OutputFormat.hpp"

#include <fstream>
#include <iostream>
#include <string>

using std::ofstream;
using std::string;

void write_output(double θ_p) {
    ofstream outputfile;
    outputfile.open("output.txt", std::fstream::out);
    outputfile << "θ_p = ";
    outputfile << θ_p << std::endl;
    outputfile.close();
}
