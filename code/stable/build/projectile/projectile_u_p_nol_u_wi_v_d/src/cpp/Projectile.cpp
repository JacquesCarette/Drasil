/** \file Projectile.cpp
    \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
    \brief Contains the entire Projectile program
*/
#include "Projectile.hpp"

#define _USE_MATH_DEFINES

#include <algorithm>
#include <fstream>
#include <iostream>
#include <limits>
#include <math.h>
#include <string>

using std::ifstream;
using std::ofstream;
using std::string;

/** \brief Controls the flow of the program
    \param argc Number of command-line arguments
    \param argv List of command-line arguments
    \return exit code
*/
int main(int argc, const char *argv[]) {
    string filename = argv[1];
    double g_vect = 9.8;
    double epsilon = 2.0e-2;
    double v_launch;
    double theta;
    double p_target;
    get_input(filename, v_launch, theta, p_target);
    input_constraints(v_launch, theta, p_target);
    double t_flight = func_t_flight(v_launch, theta, g_vect);
    double p_land = func_p_land(v_launch, theta, g_vect);
    double d_offset = func_d_offset(p_target, p_land);
    string s = func_s(p_target, epsilon, d_offset);
    write_output(s, d_offset, t_flight);
    
    return 0;
}

double func_t_flight(double v_launch, double theta, double g_vect) {
    return 2.0 * v_launch * sin(theta) / g_vect;
}

double func_p_land(double v_launch, double theta, double g_vect) {
    return 2.0 * pow(v_launch, 2.0) * sin(theta) * cos(theta) / g_vect;
}

double func_d_offset(double p_target, double p_land) {
    return p_land - p_target;
}

string func_s(double p_target, double epsilon, double d_offset) {
    if (fabs(d_offset / p_target) < epsilon) {
        return "The target was hit.";
    }
    else if (d_offset < 0.0) {
        return "The projectile fell short.";
    }
    else {
        return "The projectile went long.";
    }
}

void get_input(string filename, double &v_launch, double &theta, double &p_target) {
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

void input_constraints(double v_launch, double theta, double p_target) {
    if (!(v_launch > 0.0)) {
        std::cout << "Warning: ";
        std::cout << "v_launch has value ";
        std::cout << v_launch;
        std::cout << ", but is suggested to be ";
        std::cout << "above ";
        std::cout << 0.0;
        std::cout << "." << std::endl;
    }
    if (!(0.0 < theta && theta < M_PI / 2.0)) {
        std::cout << "Warning: ";
        std::cout << "theta has value ";
        std::cout << theta;
        std::cout << ", but is suggested to be ";
        std::cout << "between ";
        std::cout << 0.0;
        std::cout << " and ";
        std::cout << (M_PI / 2.0);
        std::cout << " ((pi)/(2))";
        std::cout << "." << std::endl;
    }
    if (!(p_target > 0.0)) {
        std::cout << "Warning: ";
        std::cout << "p_target has value ";
        std::cout << p_target;
        std::cout << ", but is suggested to be ";
        std::cout << "above ";
        std::cout << 0.0;
        std::cout << "." << std::endl;
    }
}

void write_output(string s, double d_offset, double t_flight) {
    ofstream outputfile;
    outputfile.open("output.txt", std::fstream::out);
    outputfile << "s = ";
    outputfile << s << std::endl;
    outputfile << "d_offset = ";
    outputfile << d_offset << std::endl;
    outputfile << "t_flight = ";
    outputfile << t_flight << std::endl;
    outputfile.close();
}
