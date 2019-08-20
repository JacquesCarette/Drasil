#include "Interpolation.hpp"

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

#include "ReadTable.hpp"

double func_lin_interp(double x_1, double y_1, double x_2, double y_2, double x) {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function func_lin_interp called with inputs: {" << std::endl;
    outfile << "  x_1 = ";
    outfile << x_1;
    outfile << ", " << std::endl;
    outfile << "  y_1 = ";
    outfile << y_1;
    outfile << ", " << std::endl;
    outfile << "  x_2 = ";
    outfile << x_2;
    outfile << ", " << std::endl;
    outfile << "  y_2 = ";
    outfile << y_2;
    outfile << ", " << std::endl;
    outfile << "  x = ";
    outfile << x << std::endl;
    outfile << "  }" << std::endl;
    outfile.close();
    
    return (y_2 - y_1) / (x_2 - x_1) * (x - x_1) + y_1;
}

int func_find(vector<double> &arr, double v) {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function func_find called with inputs: {" << std::endl;
    outfile << "  arr = ";
    outfile << "[";
    for (int list_i1 = 0; list_i1 < (int)(arr.size()) - 1; list_i1++) {
        outfile << arr.at(list_i1);
        outfile << ", /f ";
    }
    if ((int)(arr.size()) > 0) {
        outfile << arr.at((int)(arr.size()) - 1);
    }
    outfile << "]";
    outfile << ", " << std::endl;
    outfile << "  v = ";
    outfile << v << std::endl;
    outfile << "  }" << std::endl;
    outfile.close();
    
    for (int i = 0; i < (int)(arr.size()) - 1; i += 1) {
        if (arr.at(i) <= v && v <= arr.at(i + 1)) {
            return i;
        }
    }
    throw("Bound error");
}

vector<double> func_extractColumn(vector<vector<double>> &mat, int j) {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function func_extractColumn called with inputs: {" << std::endl;
    outfile << "  mat = ";
    outfile << "[";
    for (int list_i2 = 0; list_i2 < (int)(mat.size()) - 1; list_i2++) {
        outfile << "[";
        for (int list_i1 = 0; list_i1 < (int)(mat.at(list_i2).size()) - 1; list_i1++) {
            outfile << mat.at(list_i2).at(list_i1);
            outfile << ", /f ";
        }
        if ((int)(mat.at(list_i2).size()) > 0) {
            outfile << mat.at(list_i2).at((int)(mat.at(list_i2).size()) - 1);
        }
        outfile << "]";
        outfile << ", /f ";
    }
    if ((int)(mat.size()) > 0) {
        outfile << "[";
        for (int list_i1 = 0; list_i1 < (int)(mat.at((int)(mat.size()) - 1).size()) - 1; list_i1++) {
            outfile << mat.at((int)(mat.size()) - 1).at(list_i1);
            outfile << ", /f ";
        }
        if ((int)(mat.at((int)(mat.size()) - 1).size()) > 0) {
            outfile << mat.at((int)(mat.size()) - 1).at((int)(mat.at((int)(mat.size()) - 1).size()) - 1);
        }
        outfile << "]";
    }
    outfile << "]";
    outfile << ", " << std::endl;
    outfile << "  j = ";
    outfile << j << std::endl;
    outfile << "  }" << std::endl;
    outfile.close();
    
    vector<double> col(0);
    for (int i = 0; i < (int)(mat.size()); i += 1) {
        col.push_back(mat.at(i).at(j));
    }
    return col;
}

double func_interpY(string filename, double x, double z) {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function func_interpY called with inputs: {" << std::endl;
    outfile << "  filename = ";
    outfile << filename;
    outfile << ", " << std::endl;
    outfile << "  x = ";
    outfile << x;
    outfile << ", " << std::endl;
    outfile << "  z = ";
    outfile << z << std::endl;
    outfile << "  }" << std::endl;
    outfile.close();
    
    int i;
    vector<double> x_z_1;
    vector<double> y_z_1;
    vector<double> x_z_2;
    vector<double> y_z_2;
    int j;
    int k_2;
    double y_1;
    double y_2;
    vector<vector<double>> x_matrix(0);
    vector<vector<double>> y_matrix(0);
    vector<double> z_vector(0);
    func_read_table(filename, z_vector, x_matrix, y_matrix);
    i = func_find(z_vector, z);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'i' assigned to ";
    outfile << i;
    outfile << " in module Interpolation" << std::endl;
    outfile.close();
    x_z_1 = func_extractColumn(x_matrix, i);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'x_z_1' assigned to ";
    outfile << "[";
    for (int list_i1 = 0; list_i1 < (int)(x_z_1.size()) - 1; list_i1++) {
        outfile << x_z_1.at(list_i1);
        outfile << ", /f ";
    }
    if ((int)(x_z_1.size()) > 0) {
        outfile << x_z_1.at((int)(x_z_1.size()) - 1);
    }
    outfile << "]";
    outfile << " in module Interpolation" << std::endl;
    outfile.close();
    y_z_1 = func_extractColumn(y_matrix, i);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'y_z_1' assigned to ";
    outfile << "[";
    for (int list_i1 = 0; list_i1 < (int)(y_z_1.size()) - 1; list_i1++) {
        outfile << y_z_1.at(list_i1);
        outfile << ", /f ";
    }
    if ((int)(y_z_1.size()) > 0) {
        outfile << y_z_1.at((int)(y_z_1.size()) - 1);
    }
    outfile << "]";
    outfile << " in module Interpolation" << std::endl;
    outfile.close();
    x_z_2 = func_extractColumn(x_matrix, i + 1);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'x_z_2' assigned to ";
    outfile << "[";
    for (int list_i1 = 0; list_i1 < (int)(x_z_2.size()) - 1; list_i1++) {
        outfile << x_z_2.at(list_i1);
        outfile << ", /f ";
    }
    if ((int)(x_z_2.size()) > 0) {
        outfile << x_z_2.at((int)(x_z_2.size()) - 1);
    }
    outfile << "]";
    outfile << " in module Interpolation" << std::endl;
    outfile.close();
    y_z_2 = func_extractColumn(y_matrix, i + 1);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'y_z_2' assigned to ";
    outfile << "[";
    for (int list_i1 = 0; list_i1 < (int)(y_z_2.size()) - 1; list_i1++) {
        outfile << y_z_2.at(list_i1);
        outfile << ", /f ";
    }
    if ((int)(y_z_2.size()) > 0) {
        outfile << y_z_2.at((int)(y_z_2.size()) - 1);
    }
    outfile << "]";
    outfile << " in module Interpolation" << std::endl;
    outfile.close();
    try {
        j = func_find(x_z_1, x);
        outfile.open("log.txt", std::fstream::app);
        outfile << "var 'j' assigned to ";
        outfile << j;
        outfile << " in module Interpolation" << std::endl;
        outfile.close();
        k_2 = func_find(x_z_2, x);
        outfile.open("log.txt", std::fstream::app);
        outfile << "var 'k_2' assigned to ";
        outfile << k_2;
        outfile << " in module Interpolation" << std::endl;
        outfile.close();
    } catch (...) {
        throw("Interpolation of y failed");
    }
    y_1 = func_lin_interp(x_z_1.at(j), y_z_1.at(j), x_z_1.at(j + 1), y_z_1.at(j + 1), x);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'y_1' assigned to ";
    outfile << y_1;
    outfile << " in module Interpolation" << std::endl;
    outfile.close();
    y_2 = func_lin_interp(x_z_2.at(k_2), y_z_2.at(k_2), x_z_2.at(k_2 + 1), y_z_2.at(k_2 + 1), x);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'y_2' assigned to ";
    outfile << y_2;
    outfile << " in module Interpolation" << std::endl;
    outfile.close();
    return func_lin_interp(z_vector.at(i), y_1, z_vector.at(i + 1), y_2, z);
}

double func_interpZ(string filename, double x, double y) {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function func_interpZ called with inputs: {" << std::endl;
    outfile << "  filename = ";
    outfile << filename;
    outfile << ", " << std::endl;
    outfile << "  x = ";
    outfile << x;
    outfile << ", " << std::endl;
    outfile << "  y = ";
    outfile << y << std::endl;
    outfile << "  }" << std::endl;
    outfile.close();
    
    vector<double> x_z_1;
    vector<double> y_z_1;
    vector<double> x_z_2;
    vector<double> y_z_2;
    int j;
    int k_2;
    double y_1;
    double y_2;
    vector<vector<double>> x_matrix(0);
    vector<vector<double>> y_matrix(0);
    vector<double> z_vector(0);
    func_read_table(filename, z_vector, x_matrix, y_matrix);
    for (int i = 0; i < (int)(z_vector.size()) - 1; i += 1) {
        x_z_1 = func_extractColumn(x_matrix, i);
        outfile.open("log.txt", std::fstream::app);
        outfile << "var 'x_z_1' assigned to ";
        outfile << "[";
        for (int list_i1 = 0; list_i1 < (int)(x_z_1.size()) - 1; list_i1++) {
            outfile << x_z_1.at(list_i1);
            outfile << ", /f ";
        }
        if ((int)(x_z_1.size()) > 0) {
            outfile << x_z_1.at((int)(x_z_1.size()) - 1);
        }
        outfile << "]";
        outfile << " in module Interpolation" << std::endl;
        outfile.close();
        y_z_1 = func_extractColumn(y_matrix, i);
        outfile.open("log.txt", std::fstream::app);
        outfile << "var 'y_z_1' assigned to ";
        outfile << "[";
        for (int list_i1 = 0; list_i1 < (int)(y_z_1.size()) - 1; list_i1++) {
            outfile << y_z_1.at(list_i1);
            outfile << ", /f ";
        }
        if ((int)(y_z_1.size()) > 0) {
            outfile << y_z_1.at((int)(y_z_1.size()) - 1);
        }
        outfile << "]";
        outfile << " in module Interpolation" << std::endl;
        outfile.close();
        x_z_2 = func_extractColumn(x_matrix, i + 1);
        outfile.open("log.txt", std::fstream::app);
        outfile << "var 'x_z_2' assigned to ";
        outfile << "[";
        for (int list_i1 = 0; list_i1 < (int)(x_z_2.size()) - 1; list_i1++) {
            outfile << x_z_2.at(list_i1);
            outfile << ", /f ";
        }
        if ((int)(x_z_2.size()) > 0) {
            outfile << x_z_2.at((int)(x_z_2.size()) - 1);
        }
        outfile << "]";
        outfile << " in module Interpolation" << std::endl;
        outfile.close();
        y_z_2 = func_extractColumn(y_matrix, i + 1);
        outfile.open("log.txt", std::fstream::app);
        outfile << "var 'y_z_2' assigned to ";
        outfile << "[";
        for (int list_i1 = 0; list_i1 < (int)(y_z_2.size()) - 1; list_i1++) {
            outfile << y_z_2.at(list_i1);
            outfile << ", /f ";
        }
        if ((int)(y_z_2.size()) > 0) {
            outfile << y_z_2.at((int)(y_z_2.size()) - 1);
        }
        outfile << "]";
        outfile << " in module Interpolation" << std::endl;
        outfile.close();
        try {
            j = func_find(x_z_1, x);
            outfile.open("log.txt", std::fstream::app);
            outfile << "var 'j' assigned to ";
            outfile << j;
            outfile << " in module Interpolation" << std::endl;
            outfile.close();
            k_2 = func_find(x_z_2, x);
            outfile.open("log.txt", std::fstream::app);
            outfile << "var 'k_2' assigned to ";
            outfile << k_2;
            outfile << " in module Interpolation" << std::endl;
            outfile.close();
        } catch (...) {
            continue;
        }
        y_1 = func_lin_interp(x_z_1.at(j), y_z_1.at(j), x_z_1.at(j + 1), y_z_1.at(j + 1), x);
        outfile.open("log.txt", std::fstream::app);
        outfile << "var 'y_1' assigned to ";
        outfile << y_1;
        outfile << " in module Interpolation" << std::endl;
        outfile.close();
        y_2 = func_lin_interp(x_z_2.at(k_2), y_z_2.at(k_2), x_z_2.at(k_2 + 1), y_z_2.at(k_2 + 1), x);
        outfile.open("log.txt", std::fstream::app);
        outfile << "var 'y_2' assigned to ";
        outfile << y_2;
        outfile << " in module Interpolation" << std::endl;
        outfile.close();
        if (y_1 <= y && y <= y_2) {
            return func_lin_interp(y_1, z_vector.at(i), y_2, z_vector.at(i + 1), y);
        }
    }
    throw("Interpolation of z failed");
}

