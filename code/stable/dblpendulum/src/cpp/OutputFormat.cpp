#include "OutputFormat.hpp"

#include <fstream>
#include <iostream>
#include <string>
#include <vector>

using std::ofstream;
using std::string;
using std::vector;

void write_output(vector<double> &theta) {
    ofstream outputfile;
    outputfile.open("output.txt", std::fstream::out);
    outputfile << "theta = ";
    outputfile << "[";
    for (int list_i1 = 0; list_i1 < (int)(theta.size()) - 1; list_i1++) {
        outputfile << theta.at(list_i1);
        outputfile << ", ";
    }
    if ((int)(theta.size()) > 0) {
        outputfile << theta.at((int)(theta.size()) - 1);
    }
    outputfile << "]" << std::endl;
    outputfile.close();
}
