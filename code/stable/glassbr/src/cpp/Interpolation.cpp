#include "Interpolation.hpp"

#include <fstream>
#include <iostream>
#include <string>
#include <vector>

#include "ReadTable.hpp"

using std::ofstream;
using std::string;
using std::vector;

double lin_interp(double x_1, double y_1, double x_2, double y_2, double x) {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function lin_interp called with inputs: {" << std::endl;
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

int find(vector<double> &arr, double v) {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function find called with inputs: {" << std::endl;
    outfile << "  arr = ";
    outfile << "[";
    for (int list_i1 = 0; list_i1 < (int)(arr.size()) - 1; list_i1++) {
        outfile << arr.at(list_i1);
        outfile << ", ";
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

vector<double> extractColumn(vector<vector<double>> &mat, int j) {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function extractColumn called with inputs: {" << std::endl;
    outfile << "  mat = ";
    outfile << "[";
    for (int list_i2 = 0; list_i2 < (int)(mat.size()) - 1; list_i2++) {
        outfile << "[";
        for (int list_i1 = 0; list_i1 < (int)(mat.at(list_i2).size()) - 1; list_i1++) {
            outfile << mat.at(list_i2).at(list_i1);
            outfile << ", ";
        }
        if ((int)(mat.at(list_i2).size()) > 0) {
            outfile << mat.at(list_i2).at((int)(mat.at(list_i2).size()) - 1);
        }
        outfile << "]";
        outfile << ", ";
    }
    if ((int)(mat.size()) > 0) {
        outfile << "[";
        for (int list_i1 = 0; list_i1 < (int)(mat.at((int)(mat.size()) - 1).size()) - 1; list_i1++) {
            outfile << mat.at((int)(mat.size()) - 1).at(list_i1);
            outfile << ", ";
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

double interpY(string filename, double x, double z) {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function interpY called with inputs: {" << std::endl;
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
    vector<double> z_vect3DSor(0);
    read_table(filename, z_vect3DSor, x_matrix, y_matrix);
    i = find(z_vect3DSor, z);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'i' assigned ";
    outfile << i;
    outfile << " in module Interpolation" << std::endl;
    outfile.close();
    x_z_1 = extractColumn(x_matrix, i);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'x_z_1' assigned ";
    outfile << "[";
    for (int list_i1 = 0; list_i1 < (int)(x_z_1.size()) - 1; list_i1++) {
        outfile << x_z_1.at(list_i1);
        outfile << ", ";
    }
    if ((int)(x_z_1.size()) > 0) {
        outfile << x_z_1.at((int)(x_z_1.size()) - 1);
    }
    outfile << "]";
    outfile << " in module Interpolation" << std::endl;
    outfile.close();
    y_z_1 = extractColumn(y_matrix, i);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'y_z_1' assigned ";
    outfile << "[";
    for (int list_i1 = 0; list_i1 < (int)(y_z_1.size()) - 1; list_i1++) {
        outfile << y_z_1.at(list_i1);
        outfile << ", ";
    }
    if ((int)(y_z_1.size()) > 0) {
        outfile << y_z_1.at((int)(y_z_1.size()) - 1);
    }
    outfile << "]";
    outfile << " in module Interpolation" << std::endl;
    outfile.close();
    x_z_2 = extractColumn(x_matrix, i + 1);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'x_z_2' assigned ";
    outfile << "[";
    for (int list_i1 = 0; list_i1 < (int)(x_z_2.size()) - 1; list_i1++) {
        outfile << x_z_2.at(list_i1);
        outfile << ", ";
    }
    if ((int)(x_z_2.size()) > 0) {
        outfile << x_z_2.at((int)(x_z_2.size()) - 1);
    }
    outfile << "]";
    outfile << " in module Interpolation" << std::endl;
    outfile.close();
    y_z_2 = extractColumn(y_matrix, i + 1);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'y_z_2' assigned ";
    outfile << "[";
    for (int list_i1 = 0; list_i1 < (int)(y_z_2.size()) - 1; list_i1++) {
        outfile << y_z_2.at(list_i1);
        outfile << ", ";
    }
    if ((int)(y_z_2.size()) > 0) {
        outfile << y_z_2.at((int)(y_z_2.size()) - 1);
    }
    outfile << "]";
    outfile << " in module Interpolation" << std::endl;
    outfile.close();
    try {
        j = find(x_z_1, x);
        outfile.open("log.txt", std::fstream::app);
        outfile << "var 'j' assigned ";
        outfile << j;
        outfile << " in module Interpolation" << std::endl;
        outfile.close();
        k_2 = find(x_z_2, x);
        outfile.open("log.txt", std::fstream::app);
        outfile << "var 'k_2' assigned ";
        outfile << k_2;
        outfile << " in module Interpolation" << std::endl;
        outfile.close();
    } catch (...) {
        throw("Interpolation of y failed");
    }
    y_1 = lin_interp(x_z_1.at(j), y_z_1.at(j), x_z_1.at(j + 1), y_z_1.at(j + 1), x);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'y_1' assigned ";
    outfile << y_1;
    outfile << " in module Interpolation" << std::endl;
    outfile.close();
    y_2 = lin_interp(x_z_2.at(k_2), y_z_2.at(k_2), x_z_2.at(k_2 + 1), y_z_2.at(k_2 + 1), x);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'y_2' assigned ";
    outfile << y_2;
    outfile << " in module Interpolation" << std::endl;
    outfile.close();
    return lin_interp(z_vect3DSor.at(i), y_1, z_vect3DSor.at(i + 1), y_2, z);
}

double interpZ(string filename, double x, double y) {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function interpZ called with inputs: {" << std::endl;
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
    vector<double> z_vect3DSor(0);
    read_table(filename, z_vect3DSor, x_matrix, y_matrix);
    for (int i = 0; i < (int)(z_vect3DSor.size()) - 1; i += 1) {
        x_z_1 = extractColumn(x_matrix, i);
        outfile.open("log.txt", std::fstream::app);
        outfile << "var 'x_z_1' assigned ";
        outfile << "[";
        for (int list_i1 = 0; list_i1 < (int)(x_z_1.size()) - 1; list_i1++) {
            outfile << x_z_1.at(list_i1);
            outfile << ", ";
        }
        if ((int)(x_z_1.size()) > 0) {
            outfile << x_z_1.at((int)(x_z_1.size()) - 1);
        }
        outfile << "]";
        outfile << " in module Interpolation" << std::endl;
        outfile.close();
        y_z_1 = extractColumn(y_matrix, i);
        outfile.open("log.txt", std::fstream::app);
        outfile << "var 'y_z_1' assigned ";
        outfile << "[";
        for (int list_i1 = 0; list_i1 < (int)(y_z_1.size()) - 1; list_i1++) {
            outfile << y_z_1.at(list_i1);
            outfile << ", ";
        }
        if ((int)(y_z_1.size()) > 0) {
            outfile << y_z_1.at((int)(y_z_1.size()) - 1);
        }
        outfile << "]";
        outfile << " in module Interpolation" << std::endl;
        outfile.close();
        x_z_2 = extractColumn(x_matrix, i + 1);
        outfile.open("log.txt", std::fstream::app);
        outfile << "var 'x_z_2' assigned ";
        outfile << "[";
        for (int list_i1 = 0; list_i1 < (int)(x_z_2.size()) - 1; list_i1++) {
            outfile << x_z_2.at(list_i1);
            outfile << ", ";
        }
        if ((int)(x_z_2.size()) > 0) {
            outfile << x_z_2.at((int)(x_z_2.size()) - 1);
        }
        outfile << "]";
        outfile << " in module Interpolation" << std::endl;
        outfile.close();
        y_z_2 = extractColumn(y_matrix, i + 1);
        outfile.open("log.txt", std::fstream::app);
        outfile << "var 'y_z_2' assigned ";
        outfile << "[";
        for (int list_i1 = 0; list_i1 < (int)(y_z_2.size()) - 1; list_i1++) {
            outfile << y_z_2.at(list_i1);
            outfile << ", ";
        }
        if ((int)(y_z_2.size()) > 0) {
            outfile << y_z_2.at((int)(y_z_2.size()) - 1);
        }
        outfile << "]";
        outfile << " in module Interpolation" << std::endl;
        outfile.close();
        try {
            j = find(x_z_1, x);
            outfile.open("log.txt", std::fstream::app);
            outfile << "var 'j' assigned ";
            outfile << j;
            outfile << " in module Interpolation" << std::endl;
            outfile.close();
            k_2 = find(x_z_2, x);
            outfile.open("log.txt", std::fstream::app);
            outfile << "var 'k_2' assigned ";
            outfile << k_2;
            outfile << " in module Interpolation" << std::endl;
            outfile.close();
        } catch (...) {
            continue;
        }
        y_1 = lin_interp(x_z_1.at(j), y_z_1.at(j), x_z_1.at(j + 1), y_z_1.at(j + 1), x);
        outfile.open("log.txt", std::fstream::app);
        outfile << "var 'y_1' assigned ";
        outfile << y_1;
        outfile << " in module Interpolation" << std::endl;
        outfile.close();
        y_2 = lin_interp(x_z_2.at(k_2), y_z_2.at(k_2), x_z_2.at(k_2 + 1), y_z_2.at(k_2 + 1), x);
        outfile.open("log.txt", std::fstream::app);
        outfile << "var 'y_2' assigned ";
        outfile << y_2;
        outfile << " in module Interpolation" << std::endl;
        outfile.close();
        if (y_1 <= y && y <= y_2) {
            return lin_interp(y_1, z_vect3DSor.at(i), y_2, z_vect3DSor.at(i + 1), y);
        }
    }
    throw("Interpolation of z failed");
}
