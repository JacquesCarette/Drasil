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

InputParameters::InputParameters(string filename) {
    this->get_input(filename);
    this->input_constraints();
}

void InputParameters::get_input(string filename) {
    ifstream infile;
    infile.open(filename, std::fstream::in);
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> this->v_launch;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> this->theta;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> this->p_target;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.close();
}

void InputParameters::input_constraints() {
    if (!(this->v_launch > 0.0)) {
        std::cout << "Warning: ";
        std::cout << "v_launch has value ";
        std::cout << this->v_launch;
        std::cout << ", but is suggested to be ";
        std::cout << "above ";
        std::cout << 0.0;
        std::cout << "." << std::endl;
    }
    if (!(0.0 < this->theta && this->theta < M_PI / 2.0)) {
        std::cout << "Warning: ";
        std::cout << "theta has value ";
        std::cout << this->theta;
        std::cout << ", but is suggested to be ";
        std::cout << "between ";
        std::cout << 0.0;
        std::cout << " and ";
        std::cout << (M_PI / 2.0);
        std::cout << " ((pi)/(2))";
        std::cout << "." << std::endl;
    }
    if (!(this->p_target > 0.0)) {
        std::cout << "Warning: ";
        std::cout << "p_target has value ";
        std::cout << this->p_target;
        std::cout << ", but is suggested to be ";
        std::cout << "above ";
        std::cout << 0.0;
        std::cout << "." << std::endl;
    }
}
