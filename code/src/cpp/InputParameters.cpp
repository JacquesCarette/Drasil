#include "InputParameters.hpp"

#include <algorithm>
#include <fstream>
#include <iostream>
#include <limits>
#include <string>

using std::ifstream;
using std::string;

void get_input(string filename, double &L_1, double &L_2, double &m_1, double &m_2) {
    ifstream infile;
    infile.open(filename, std::fstream::in);
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> L_1;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> L_2;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> m_1;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> m_2;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.close();
}

void input_constraints(double L_1, double L_2, double m_1, double m_2) {
    if (!(L_1 > 0.0)) {
        std::cout << "Warning: ";
        std::cout << "L_1 has value ";
        std::cout << L_1;
        std::cout << ", but is suggested to be ";
        std::cout << "above ";
        std::cout << 0.0;
        std::cout << "." << std::endl;
    }
    if (!(L_2 > 0.0)) {
        std::cout << "Warning: ";
        std::cout << "L_2 has value ";
        std::cout << L_2;
        std::cout << ", but is suggested to be ";
        std::cout << "above ";
        std::cout << 0.0;
        std::cout << "." << std::endl;
    }
    if (!(m_1 > 0.0)) {
        std::cout << "Warning: ";
        std::cout << "m_1 has value ";
        std::cout << m_1;
        std::cout << ", but is suggested to be ";
        std::cout << "above ";
        std::cout << 0.0;
        std::cout << "." << std::endl;
    }
    if (!(m_2 > 0.0)) {
        std::cout << "Warning: ";
        std::cout << "m_2 has value ";
        std::cout << m_2;
        std::cout << ", but is suggested to be ";
        std::cout << "above ";
        std::cout << 0.0;
        std::cout << "." << std::endl;
    }
}
