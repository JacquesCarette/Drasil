#include "OutputFormat.hpp"

#include <fstream>
#include <iostream>
#include <string>
#include <vector>

using std::ofstream;
using std::string;
using std::vector;

void write_output(double E_W, vector<double> &T_W) {
    ofstream outputfile;
    outputfile.open("output.txt", std::fstream::out);
    outputfile << "T_W = ";
    outputfile << "[";
    for (int list_i1 = 0; list_i1 < (int)(T_W.size()) - 1; list_i1++) {
        outputfile << T_W.at(list_i1);
        outputfile << ", ";
    }
    if ((int)(T_W.size()) > 0) {
        outputfile << T_W.at((int)(T_W.size()) - 1);
    }
    outputfile << "]" << std::endl;
    outputfile << "E_W = ";
    outputfile << E_W << std::endl;
    outputfile.close();
}
