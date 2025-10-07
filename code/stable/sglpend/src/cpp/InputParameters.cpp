#include "InputParameters.hpp"

#include <algorithm>
#include <fstream>
#include <iostream>
#include <limits>
#include <math.h>
#include <string>

using std::ifstream;
using std::string;

void get_input(string filename, double &L_rod, double &m, double &α, double &θ_p, double &θ_i) {
    ifstream infile;
    infile.open(filename, std::fstream::in);
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> L_rod;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> m;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> α;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> θ_p;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> θ_i;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.close();
}

void derived_values(double L_rod, double θ_i, double &p_x^i, double &p_y^i) {
    p_x^i = L_rod * sin(θ_i);
    
    p_y^i = -(L_rod * cos(θ_i));
}

void input_constraints(double L_rod, double θ_i) {
    if (!(L_rod > 0.0)) {
        std::cout << "Warning: ";
        std::cout << "L_rod has value ";
        std::cout << L_rod;
        std::cout << ", but is suggested to be ";
        std::cout << "above ";
        std::cout << 0.0;
        std::cout << "." << std::endl;
    }
    if (!(θ_i > 0.0)) {
        std::cout << "Warning: ";
        std::cout << "θ_i has value ";
        std::cout << θ_i;
        std::cout << ", but is suggested to be ";
        std::cout << "above ";
        std::cout << 0.0;
        std::cout << "." << std::endl;
    }
}
