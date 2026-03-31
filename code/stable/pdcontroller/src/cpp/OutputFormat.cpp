#include "OutputFormat.hpp"

#include <fstream>
#include <iostream>
#include <string>
#include <vector>

using std::ofstream;
using std::string;
using std::vector;

void write_output(vector<vector<double>> &y_t) {
    ofstream outputfile;
    outputfile.open("output.txt", std::fstream::out);
    outputfile << "y_t = ";
    outputfile << "[";
    for (int list_i1 = 0; list_i1 < (int)(y_t.size()); list_i1 += 1) {
        outputfile << "[";
        for (int list_i2 = 0; list_i2 < (int)(y_t.at(list_i1).size()); list_i2 += 1) {
            outputfile << y_t.at(list_i1).at(list_i2);
            if (list_i2 < (int)(y_t.at(list_i1).size()) - 1) {
                outputfile << ", ";
            }
        }
        outputfile << "]";
        if (list_i1 < (int)(y_t.size()) - 1) {
            outputfile << ", ";
        }
    }
    outputfile << "]";
    outputfile << "" << std::endl;
    outputfile.close();
}
