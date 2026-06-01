#include "InputParameters.hpp"

#define _USE_MATH_DEFINES

#include <algorithm>
#include <fstream>
#include <iostream>
#include <limits>
#include <math.h>
#include <string>

using std::ifstream;
using std::string;

void get_input(string filename, float &v_launch, float &theta, float &p_target) {
    ifstream infile;
    infile.open(filename, std::fstream::in);
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> v_launch;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> theta;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> p_target;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.close();
}

void input_constraints(float v_launch, float theta, float p_target) {
    if (!(v_launch > 0.0f)) {
        std::cout << "Warning: ";
        std::cout << "v_launch has value ";
        std::cout << v_launch;
        std::cout << ", but is suggested to be ";
        std::cout << "above ";
        std::cout << 0.0f;
        std::cout << "." << std::endl;
    }
    if (!(0.0f < theta && theta < M_PI / 2.0f)) {
        std::cout << "Warning: ";
        std::cout << "theta has value ";
        std::cout << theta;
        std::cout << ", but is suggested to be ";
        std::cout << "between ";
        std::cout << 0.0f;
        std::cout << " and ";
        std::cout << (M_PI / 2.0f);
        std::cout << " ((pi)/(2))";
        std::cout << "." << std::endl;
    }
    if (!(p_target > 0.0f)) {
        std::cout << "Warning: ";
        std::cout << "p_target has value ";
        std::cout << p_target;
        std::cout << ", but is suggested to be ";
        std::cout << "above ";
        std::cout << 0.0f;
        std::cout << "." << std::endl;
    }
}
