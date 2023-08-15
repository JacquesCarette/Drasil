#include "InputParameters.hpp"

#include <algorithm>
#include <fstream>
#include <iostream>
#include <limits>
#include <string>

using std::ifstream;
using std::string;

void get_input(string filename, double &r_t, double &K_d, double &K_p, double &t_step, double &t_sim) {
    ifstream infile;
    infile.open(filename, std::fstream::in);
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> r_t;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> K_d;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> K_p;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> t_step;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> t_sim;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.close();
}

void input_constraints(double r_t, double K_d, double K_p, double t_step, double t_sim) {
    if (!(r_t > 0.0)) {
        std::cout << "r_t has value ";
        std::cout << r_t;
        std::cout << ", but is expected to be ";
        std::cout << "above ";
        std::cout << 0.0;
        std::cout << "." << std::endl;
        throw("InputError");
    }
    if (!(K_d >= 0.0)) {
        std::cout << "K_d has value ";
        std::cout << K_d;
        std::cout << ", but is expected to be ";
        std::cout << "above ";
        std::cout << 0.0;
        std::cout << "." << std::endl;
        throw("InputError");
    }
    if (!(K_p > 0.0)) {
        std::cout << "K_p has value ";
        std::cout << K_p;
        std::cout << ", but is expected to be ";
        std::cout << "above ";
        std::cout << 0.0;
        std::cout << "." << std::endl;
        throw("InputError");
    }
    if (!(1.0 / 1000.0 <= t_step && t_step < t_sim)) {
        std::cout << "t_step has value ";
        std::cout << t_step;
        std::cout << ", but is expected to be ";
        std::cout << "between ";
        std::cout << (1.0 / 1000.0);
        std::cout << " ((1)/(1000))";
        std::cout << " and ";
        std::cout << t_sim;
        std::cout << " (t_sim)";
        std::cout << "." << std::endl;
        throw("InputError");
    }
    if (!(1.0 <= t_sim && t_sim <= 60.0)) {
        std::cout << "t_sim has value ";
        std::cout << t_sim;
        std::cout << ", but is expected to be ";
        std::cout << "between ";
        std::cout << 1.0;
        std::cout << " and ";
        std::cout << 60.0;
        std::cout << "." << std::endl;
        throw("InputError");
    }
}
