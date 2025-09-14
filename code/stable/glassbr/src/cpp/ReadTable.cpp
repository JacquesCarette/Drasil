#include "ReadTable.hpp"

#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

using std::ifstream;
using std::ofstream;
using std::string;
using std::vector;

void read_table(string filename, vector<double> &z_vect3DSor, vector<vector<double>> &x_matrix, vector<vector<double>> &y_matrix) {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function read_table called with inputs: {" << std::endl;
    outfile << "  filename = ";
    outfile << filename;
    outfile << ", " << std::endl;
    outfile << "  z_vect3DSor = ";
    outfile << "[";
    for (int list_i1 = 0; list_i1 < (int)(z_vect3DSor.size()) - 1; list_i1++) {
        outfile << z_vect3DSor.at(list_i1);
        outfile << ", ";
    }
    if ((int)(z_vect3DSor.size()) > 0) {
        outfile << z_vect3DSor.at((int)(z_vect3DSor.size()) - 1);
    }
    outfile << "]";
    outfile << ", " << std::endl;
    outfile << "  x_matrix = ";
    outfile << "[";
    for (int list_i2 = 0; list_i2 < (int)(x_matrix.size()) - 1; list_i2++) {
        outfile << "[";
        for (int list_i1 = 0; list_i1 < (int)(x_matrix.at(list_i2).size()) - 1; list_i1++) {
            outfile << x_matrix.at(list_i2).at(list_i1);
            outfile << ", ";
        }
        if ((int)(x_matrix.at(list_i2).size()) > 0) {
            outfile << x_matrix.at(list_i2).at((int)(x_matrix.at(list_i2).size()) - 1);
        }
        outfile << "]";
        outfile << ", ";
    }
    if ((int)(x_matrix.size()) > 0) {
        outfile << "[";
        for (int list_i1 = 0; list_i1 < (int)(x_matrix.at((int)(x_matrix.size()) - 1).size()) - 1; list_i1++) {
            outfile << x_matrix.at((int)(x_matrix.size()) - 1).at(list_i1);
            outfile << ", ";
        }
        if ((int)(x_matrix.at((int)(x_matrix.size()) - 1).size()) > 0) {
            outfile << x_matrix.at((int)(x_matrix.size()) - 1).at((int)(x_matrix.at((int)(x_matrix.size()) - 1).size()) - 1);
        }
        outfile << "]";
    }
    outfile << "]";
    outfile << ", " << std::endl;
    outfile << "  y_matrix = ";
    outfile << "[";
    for (int list_i2 = 0; list_i2 < (int)(y_matrix.size()) - 1; list_i2++) {
        outfile << "[";
        for (int list_i1 = 0; list_i1 < (int)(y_matrix.at(list_i2).size()) - 1; list_i1++) {
            outfile << y_matrix.at(list_i2).at(list_i1);
            outfile << ", ";
        }
        if ((int)(y_matrix.at(list_i2).size()) > 0) {
            outfile << y_matrix.at(list_i2).at((int)(y_matrix.at(list_i2).size()) - 1);
        }
        outfile << "]";
        outfile << ", ";
    }
    if ((int)(y_matrix.size()) > 0) {
        outfile << "[";
        for (int list_i1 = 0; list_i1 < (int)(y_matrix.at((int)(y_matrix.size()) - 1).size()) - 1; list_i1++) {
            outfile << y_matrix.at((int)(y_matrix.size()) - 1).at(list_i1);
            outfile << ", ";
        }
        if ((int)(y_matrix.at((int)(y_matrix.size()) - 1).size()) > 0) {
            outfile << y_matrix.at((int)(y_matrix.size()) - 1).at((int)(y_matrix.at((int)(y_matrix.size()) - 1).size()) - 1);
        }
        outfile << "]";
    }
    outfile << "]" << std::endl;
    outfile << "  }" << std::endl;
    outfile.close();
    
    ifstream infile;
    string line;
    vector<string> linetokens(0);
    vector<string> lines(0);
    infile.open(filename, std::fstream::in);
    std::getline(infile, line);
    linetokens.clear();
    std::stringstream ss;
    ss.str(line);
    string word;
    while (std::getline(ss, word, ',')) {
        linetokens.push_back(word);
    }
    for (int i = 0; i < (int)(linetokens.size()) / 1; i += 1) {
        z_vect3DSor.push_back(std::stod(linetokens.at(i * 1 + 0)));
    }
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'z_vect3DSor' assigned ";
    outfile << "[";
    for (int list_i1 = 0; list_i1 < (int)(z_vect3DSor.size()) - 1; list_i1++) {
        outfile << z_vect3DSor.at(list_i1);
        outfile << ", ";
    }
    if ((int)(z_vect3DSor.size()) > 0) {
        outfile << z_vect3DSor.at((int)(z_vect3DSor.size()) - 1);
    }
    outfile << "]";
    outfile << " in module ReadTable" << std::endl;
    outfile.close();
    string nextLine;
    while (std::getline(infile, nextLine)) {
        lines.push_back(nextLine);
    }
    for (int i = 0; i < (int)(lines.size()); i += 1) {
        linetokens.clear();
        std::stringstream ss;
        ss.str(lines.at(i));
        string word;
        while (std::getline(ss, word, ',')) {
            linetokens.push_back(word);
        }
        vector<double> x_matrix_temp{};
        vector<double> y_matrix_temp{};
        for (int j = 0; j < (int)(linetokens.size()) / 2; j += 1) {
            x_matrix_temp.push_back(std::stod(linetokens.at(j * 2 + 0)));
            y_matrix_temp.push_back(std::stod(linetokens.at(j * 2 + 1)));
        }
        x_matrix.push_back(x_matrix_temp);
        y_matrix.push_back(y_matrix_temp);
    }
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'x_matrix' assigned ";
    outfile << "[";
    for (int list_i2 = 0; list_i2 < (int)(x_matrix.size()) - 1; list_i2++) {
        outfile << "[";
        for (int list_i1 = 0; list_i1 < (int)(x_matrix.at(list_i2).size()) - 1; list_i1++) {
            outfile << x_matrix.at(list_i2).at(list_i1);
            outfile << ", ";
        }
        if ((int)(x_matrix.at(list_i2).size()) > 0) {
            outfile << x_matrix.at(list_i2).at((int)(x_matrix.at(list_i2).size()) - 1);
        }
        outfile << "]";
        outfile << ", ";
    }
    if ((int)(x_matrix.size()) > 0) {
        outfile << "[";
        for (int list_i1 = 0; list_i1 < (int)(x_matrix.at((int)(x_matrix.size()) - 1).size()) - 1; list_i1++) {
            outfile << x_matrix.at((int)(x_matrix.size()) - 1).at(list_i1);
            outfile << ", ";
        }
        if ((int)(x_matrix.at((int)(x_matrix.size()) - 1).size()) > 0) {
            outfile << x_matrix.at((int)(x_matrix.size()) - 1).at((int)(x_matrix.at((int)(x_matrix.size()) - 1).size()) - 1);
        }
        outfile << "]";
    }
    outfile << "]";
    outfile << " in module ReadTable" << std::endl;
    outfile.close();
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'y_matrix' assigned ";
    outfile << "[";
    for (int list_i2 = 0; list_i2 < (int)(y_matrix.size()) - 1; list_i2++) {
        outfile << "[";
        for (int list_i1 = 0; list_i1 < (int)(y_matrix.at(list_i2).size()) - 1; list_i1++) {
            outfile << y_matrix.at(list_i2).at(list_i1);
            outfile << ", ";
        }
        if ((int)(y_matrix.at(list_i2).size()) > 0) {
            outfile << y_matrix.at(list_i2).at((int)(y_matrix.at(list_i2).size()) - 1);
        }
        outfile << "]";
        outfile << ", ";
    }
    if ((int)(y_matrix.size()) > 0) {
        outfile << "[";
        for (int list_i1 = 0; list_i1 < (int)(y_matrix.at((int)(y_matrix.size()) - 1).size()) - 1; list_i1++) {
            outfile << y_matrix.at((int)(y_matrix.size()) - 1).at(list_i1);
            outfile << ", ";
        }
        if ((int)(y_matrix.at((int)(y_matrix.size()) - 1).size()) > 0) {
            outfile << y_matrix.at((int)(y_matrix.size()) - 1).at((int)(y_matrix.at((int)(y_matrix.size()) - 1).size()) - 1);
        }
        outfile << "]";
    }
    outfile << "]";
    outfile << " in module ReadTable" << std::endl;
    outfile.close();
    infile.close();
}
