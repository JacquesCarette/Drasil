#include "Interpolation.hpp"


#include <algorithm>
#include <iostream>
#include <fstream>
#include <iterator>
#include <string>
#include <math.h>
#include <sstream>
#include <vector>

using namespace GlassBR_program;
using std::string;
using std::vector;
using std::ifstream;
using std::ofstream;

double GlassBR_program::lin_interp(double x1, double y1, double x2, double y2, double x) {
    double y = (((y2 - y1) / (x2 - x1)) * (x - x1)) + y1;
    return y;
}

int GlassBR_program::indInSeq(vector<double> &arr, double v) {
    for (int i = 0; i < (arr.size() - 1); i++) {
        if ((arr.at(i) <= v) && (v <= arr.at(i + 1))) {
            return i;
        }
    }
    throw("Index not found");
}

vector<double> GlassBR_program::matrixCol(vector< vector<double> > &mat, int c) {
    vector<double> col(0);
    for (int i = 0; i < mat.size(); i++) {
        col.push_back(mat.at(i).at(c));
    }
    return col;
}

double GlassBR_program::interpY(vector< vector<double> > &x_array, vector< vector<double> > &y_array, vector<double> &z_array, double x, double z) {
    int i = indInSeq(z_array, z);
    vector<double> x_z1 = matrixCol(x_array, i);
    vector<double> y_z1 = matrixCol(y_array, i);
    vector<double> x_z2 = matrixCol(x_array, i + 1);
    vector<double> y_z2 = matrixCol(y_array, i + 1);
    int j;
    int k;
    try {
        j = indInSeq(x_z1, x);
        k = indInSeq(x_z2, x);
    } catch (...) {
        throw("Interpolation of y failed.");
    };
    double y1 = lin_interp(x_z1.at(j), y_z1.at(j), x_z1.at(j + 1), y_z1.at(j + 1), x);
    double y2 = lin_interp(x_z2.at(k), y_z2.at(k), x_z2.at(k + 1), y_z2.at(k + 1), x);
    return lin_interp(z_array.at(i), y1, z_array.at(i + 1), y2, z);
}

double GlassBR_program::interpZ(vector< vector<double> > &x_array, vector< vector<double> > &y_array, vector<double> &z_array, double x, double y) {
    for (int i = 0; i < (z_array.size() - 1); i++) {
        vector<double> x_z1 = matrixCol(x_array, i);
        vector<double> y_z1 = matrixCol(y_array, i);
        vector<double> x_z2 = matrixCol(x_array, i + 1);
        vector<double> y_z2 = matrixCol(y_array, i + 1);
        int j;
        int k;
        try {
            j = indInSeq(x_z1, x);
            k = indInSeq(x_z2, x);
        } catch (...) {
            continue;
        };
        double y_lower = lin_interp(x_z1.at(j), y_z1.at(j), x_z1.at(j + 1), y_z1.at(j + 1), x);
        double y_upper = lin_interp(x_z2.at(k), y_z2.at(k), x_z2.at(k + 1), y_z2.at(k + 1), x);
        if ((y_lower <= y) && (y <= y_upper)) {
            return lin_interp(y_lower, z_array.at(i), y_upper, z_array.at(i + 1), y);
        }
    }
    throw("Interpolation of z failed.");
}

