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

void func_read_table(string filename, vector<double> &z_vector, vector< vector<double> > &x_matrix, vector< vector<double> > &y_matrix) {
    ifstream infile;
    string line;
    vector<string> lines(0);
    vector<string> linetokens(0);
    infile.open(filename, std::fstream::in);
    std::getline(infile, line);
    {   linetokens.clear();
        std::stringstream ss;
        ss.str(line);
        string word;
        while (std::getline(ss, word, ',')) {
            linetokens.push_back(word);
        }
    }
    for (int j = 0; j < (int)(linetokens.size() / 2); j++) {
        while (z_vector.size() <= j) {
            z_vector.push_back(0.0);
        }
        z_vector.at(j) = std::stod(linetokens.at((j * 2) + 1));
    }
    string nextLine;
    while (std::getline(infile, nextLine)) {
        lines.push_back(nextLine);
    }
    for (int i = 0; i < lines.size(); i++) {
        {   linetokens.clear();
            std::stringstream ss;
            ss.str(lines.at(i));
            string word;
            while (std::getline(ss, word, ',')) {
                linetokens.push_back(word);
            }
        }
        for (int j = 0; j < (int)(linetokens.size() / 2); j++) {
            while (x_matrix.size() <= i) {
                x_matrix.push_back(vector<double>());
            }
            while (x_matrix.at(i).size() <= j) {
                x_matrix.at(i).push_back(0.0);
            }
            x_matrix.at(i).at(j) = std::stod(linetokens.at((j * 2) + 0));
            while (y_matrix.size() <= i) {
                y_matrix.push_back(vector<double>());
            }
            while (y_matrix.at(i).size() <= j) {
                y_matrix.at(i).push_back(0.0);
            }
            y_matrix.at(i).at(j) = std::stod(linetokens.at((j * 2) + 1));
        }
    }
    infile.close();
}

