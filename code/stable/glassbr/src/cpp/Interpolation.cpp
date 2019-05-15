#include "Interpolation.hpp"

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

double func_lin_interp(double x_1, double y_1, double x_2, double y_2, double x) {
    return (((y_2 - y_1) / (x_2 - x_1)) * (x - x_1)) + y_1;
}

int func_find(vector<double> &arr, double v) {
    for (int i = 0; i < (arr.size() - 1); i++) {
        if ((arr.at(i) <= v) && (v <= arr.at(i + 1))) {
            return i;
        }
    }
    throw("Bound error");
}

vector<double> func_extractColumn(vector< vector<double> > &mat, int j) {
    vector<double> col(0);
    for (int i = 0; i < mat.size(); i++) {
        col.push_back(mat.at(i).at(j));
    }
    return col;
}

double func_interpY(string filename, double x, double z) {
    int i;
    vector<double> x_z_1;
    vector<double> y_z_1;
    vector<double> x_z_2;
    vector<double> y_z_2;
    int j;
    int k_2;
    double y_1;
    double y_2;
    vector< vector<double> > x_matrix(0);
    vector< vector<double> > y_matrix(0);
    vector<double> z_vector(0);
    func_read_table(filename, z_vector, x_matrix, y_matrix);
    i = func_find(z_vector, z);
    x_z_1 = func_extractColumn(x_matrix, i);
    y_z_1 = func_extractColumn(y_matrix, i);
    x_z_2 = func_extractColumn(x_matrix, i + 1);
    y_z_2 = func_extractColumn(y_matrix, i + 1);
    try {
        j = func_find(x_z_1, x);
        k_2 = func_find(x_z_2, x);
    } catch (...) {
        throw("Interpolation of y failed");
    };
    y_1 = func_lin_interp(x_z_1.at(j), y_z_1.at(j), x_z_1.at(j + 1), y_z_1.at(j + 1), x);
    y_2 = func_lin_interp(x_z_2.at(k_2), y_z_2.at(k_2), x_z_2.at(k_2 + 1), y_z_2.at(k_2 + 1), x);
    return func_lin_interp(z_vector.at(i), y_1, z_vector.at(i + 1), y_2, z);
}

double func_interpZ(string filename, double x, double y) {
    vector<double> x_z_1;
    vector<double> y_z_1;
    vector<double> x_z_2;
    vector<double> y_z_2;
    int j;
    int k_2;
    double y_1;
    double y_2;
    vector< vector<double> > x_matrix(0);
    vector< vector<double> > y_matrix(0);
    vector<double> z_vector(0);
    func_read_table(filename, z_vector, x_matrix, y_matrix);
    for (int i = 0; i < (z_vector.size() - 1); i++) {
        x_z_1 = func_extractColumn(x_matrix, i);
        y_z_1 = func_extractColumn(y_matrix, i);
        x_z_2 = func_extractColumn(x_matrix, i + 1);
        y_z_2 = func_extractColumn(y_matrix, i + 1);
        try {
            j = func_find(x_z_1, x);
            k_2 = func_find(x_z_2, x);
        } catch (...) {
            continue;
        };
        y_1 = func_lin_interp(x_z_1.at(j), y_z_1.at(j), x_z_1.at(j + 1), y_z_1.at(j + 1), x);
        y_2 = func_lin_interp(x_z_2.at(k_2), y_z_2.at(k_2), x_z_2.at(k_2 + 1), y_z_2.at(k_2 + 1), x);
        if ((y_1 <= y) && (y <= y_2)) {
            return func_lin_interp(y_1, z_vector.at(i), y_2, z_vector.at(i + 1), y);
        }
    }
    throw("Interpolation of z failed");
}

