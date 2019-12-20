#include "OutputFormat.hpp"

#include <fstream>
#include <iostream>
#include <string>

using std::ofstream;
using std::string;

void write_output(string s, double d_offset) {
    ofstream outputfile;
    outputfile.open("output.txt", std::fstream::out);
    outputfile << "s = ";
    outputfile << s << std::endl;
    outputfile << "d_offset = ";
    outputfile << d_offset << std::endl;
    outputfile.close();
}
