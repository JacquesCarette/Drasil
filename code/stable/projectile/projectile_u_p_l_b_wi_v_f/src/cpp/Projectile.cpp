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

InputParameters::InputParameters(string filename) {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function InputParameters called with inputs: {" << std::endl;
    outfile << "  filename = ";
    outfile << filename << std::endl;
    outfile << "  }" << std::endl;
    outfile.close();
    
    this->get_input(filename);
    this->input_constraints();
}

void InputParameters::get_input(string filename) {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function get_input called with inputs: {" << std::endl;
    outfile << "  filename = ";
    outfile << filename << std::endl;
    outfile << "  }" << std::endl;
    outfile.close();
    
    ifstream infile;
    infile.open(filename, std::fstream::in);
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> this->v_launch;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'this->v_launch' assigned ";
    outfile << this->v_launch;
    outfile << " in module Projectile" << std::endl;
    outfile.close();
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> this->theta;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'this->theta' assigned ";
    outfile << this->theta;
    outfile << " in module Projectile" << std::endl;
    outfile.close();
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> this->p_target;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'this->p_target' assigned ";
    outfile << this->p_target;
    outfile << " in module Projectile" << std::endl;
    outfile.close();
    infile.close();
}

void InputParameters::input_constraints() {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function input_constraints called with inputs: {" << std::endl;
    outfile << "  }" << std::endl;
    outfile.close();
    
    if (!(this->v_launch > 0.0f)) {
        std::cout << "Warning: ";
        std::cout << "v_launch has value ";
        std::cout << this->v_launch;
        std::cout << ", but is suggested to be ";
        std::cout << "above ";
        std::cout << 0.0f;
        std::cout << "." << std::endl;
    }
    if (!(0.0f < this->theta && this->theta < M_PI / 2.0f)) {
        std::cout << "Warning: ";
        std::cout << "theta has value ";
        std::cout << this->theta;
        std::cout << ", but is suggested to be ";
        std::cout << "between ";
        std::cout << 0.0f;
        std::cout << " and ";
        std::cout << (M_PI / 2.0f);
        std::cout << " ((pi)/(2))";
        std::cout << "." << std::endl;
    }
    if (!(this->p_target > 0.0f)) {
        std::cout << "Warning: ";
        std::cout << "p_target has value ";
        std::cout << this->p_target;
        std::cout << ", but is suggested to be ";
        std::cout << "above ";
        std::cout << 0.0f;
        std::cout << "." << std::endl;
    }
}

/** \brief Controls the flow of the program
    \param argc Number of command-line arguments
    \param argv List of command-line arguments
    \return exit code
*/
int main(int argc, const char *argv[]) {
    ofstream outfile;
    string filename = argv[1];
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'filename' assigned ";
    outfile << filename;
    outfile << " in module Projectile" << std::endl;
    outfile.close();
    InputParameters inParams = InputParameters(filename);
    float t_flight = func_t_flight(inParams);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 't_flight' assigned ";
    outfile << t_flight;
    outfile << " in module Projectile" << std::endl;
    outfile.close();
    float p_land = func_p_land(inParams);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'p_land' assigned ";
    outfile << p_land;
    outfile << " in module Projectile" << std::endl;
    outfile.close();
    float d_offset = func_d_offset(inParams, p_land);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'd_offset' assigned ";
    outfile << d_offset;
    outfile << " in module Projectile" << std::endl;
    outfile.close();
    string s = func_s(inParams, d_offset);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 's' assigned ";
    outfile << s;
    outfile << " in module Projectile" << std::endl;
    outfile.close();
    write_output(s, d_offset, t_flight);
    
    return 0;
}

float func_t_flight(InputParameters &inParams) {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function func_t_flight called with inputs: {" << std::endl;
    outfile << "  inParams = ";
    outfile << "Instance of InputParameters object" << std::endl;
    outfile << "  }" << std::endl;
    outfile.close();
    
    return 2.0f * inParams.v_launch * sin(inParams.theta) / inParams.g_vect;
}

float func_p_land(InputParameters &inParams) {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function func_p_land called with inputs: {" << std::endl;
    outfile << "  inParams = ";
    outfile << "Instance of InputParameters object" << std::endl;
    outfile << "  }" << std::endl;
    outfile.close();
    
    return 2.0f * pow(inParams.v_launch, 2.0f) * sin(inParams.theta) * cos(inParams.theta) / inParams.g_vect;
}

float func_d_offset(InputParameters &inParams, float p_land) {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function func_d_offset called with inputs: {" << std::endl;
    outfile << "  inParams = ";
    outfile << "Instance of InputParameters object";
    outfile << ", " << std::endl;
    outfile << "  p_land = ";
    outfile << p_land << std::endl;
    outfile << "  }" << std::endl;
    outfile.close();
    
    return p_land - inParams.p_target;
}

string func_s(InputParameters &inParams, float d_offset) {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function func_s called with inputs: {" << std::endl;
    outfile << "  inParams = ";
    outfile << "Instance of InputParameters object";
    outfile << ", " << std::endl;
    outfile << "  d_offset = ";
    outfile << d_offset << std::endl;
    outfile << "  }" << std::endl;
    outfile.close();
    
    if (fabs(d_offset / inParams.p_target) < inParams.epsilon) {
        return "The target was hit.";
    }
    else if (d_offset < 0.0f) {
        return "The projectile fell short.";
    }
    else {
        return "The projectile went long.";
    }
}

void write_output(string s, float d_offset, float t_flight) {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function write_output called with inputs: {" << std::endl;
    outfile << "  s = ";
    outfile << s;
    outfile << ", " << std::endl;
    outfile << "  d_offset = ";
    outfile << d_offset;
    outfile << ", " << std::endl;
    outfile << "  t_flight = ";
    outfile << t_flight << std::endl;
    outfile << "  }" << std::endl;
    outfile.close();
    
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
