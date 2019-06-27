#include "ReadTable.hpp"

#include <algorithm>
#include <iostream>
#include <fstream>
#include <iterator>
#include <string>
#include <math.h>
#include <sstream>
#include <limits>
#include <vector>

using std::string;
using std::vector;
using std::ifstream;
using std::ofstream;

void func_read_table(string filename, vector<double> &z_vector, vector<vector<double>> &x_matrix, vector<vector<double>> &y_matrix) {
    ifstream infile;
    string line;
    vector<string> lines(0);
    vector<string> linetokens(0);
    infile.open(filename, std::fstream::in);
    std::getline(infile, line);
    linetokens.clear();
    std::stringstream ss;
    ss.str(line);
    string word;
    while (std::getline(ss, word, ',')) {
        linetokens.push_back(word);
    }
    for (int j = 0; (j < (int)((linetokens.size() / 2))); j += 1) {
        z_vector.push_back(std::stod(linetokens.at(((j * 2) + 1))));
    }
    string nextLine;
    while (std::getline(infile, nextLine)) {
        lines.push_back(nextLine);
    }
    for (int i = 0; (i < lines.size()); i += 1) {
        linetokens.clear();
        std::stringstream ss;
        ss.str(lines.at(i));
        string word;
        while (std::getline(ss, word, ',')) {
            linetokens.push_back(word);
        }
        vector<double> x_matrix_temp{};
        vector<double> y_matrix_temp{};
        for (int j = 0; (j < (int)((linetokens.size() / 2))); j += 1) {
            x_matrix_temp.push_back(std::stod(linetokens.at(((j * 2) + 0))));
            y_matrix_temp.push_back(std::stod(linetokens.at(((j * 2) + 1))));
        }
        x_matrix.push_back(x_matrix_temp);
        y_matrix.push_back(y_matrix_temp);
    }
    infile.close();
}

