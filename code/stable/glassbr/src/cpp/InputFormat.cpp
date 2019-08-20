#include "InputFormat.hpp"

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

#include "InputParameters.hpp"

void get_input(string filename, InputParameters &inParams) {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function get_input called with inputs: {" << std::endl;
    outfile << "  filename = ";
    outfile << filename;
    outfile << ", " << std::endl;
    outfile << "  inParams = ";
    outfile << "Instance of InputParameters object" << std::endl;
    outfile << "  }" << std::endl;
    outfile.close();
    
    ifstream infile;
    infile.open(filename, std::fstream::in);
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> inParams.a;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'inParams.a' assigned to ";
    outfile << inParams.a;
    outfile << " in module InputFormat" << std::endl;
    outfile.close();
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> inParams.b;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'inParams.b' assigned to ";
    outfile << inParams.b;
    outfile << " in module InputFormat" << std::endl;
    outfile.close();
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> inParams.w;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'inParams.w' assigned to ";
    outfile << inParams.w;
    outfile << " in module InputFormat" << std::endl;
    outfile.close();
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> inParams.P_btol;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'inParams.P_btol' assigned to ";
    outfile << inParams.P_btol;
    outfile << " in module InputFormat" << std::endl;
    outfile.close();
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> inParams.TNT;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'inParams.TNT' assigned to ";
    outfile << inParams.TNT;
    outfile << " in module InputFormat" << std::endl;
    outfile.close();
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> inParams.g;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'inParams.g' assigned to ";
    outfile << inParams.g;
    outfile << " in module InputFormat" << std::endl;
    outfile.close();
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> inParams.t;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'inParams.t' assigned to ";
    outfile << inParams.t;
    outfile << " in module InputFormat" << std::endl;
    outfile.close();
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> inParams.SD_x;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'inParams.SD_x' assigned to ";
    outfile << inParams.SD_x;
    outfile << " in module InputFormat" << std::endl;
    outfile.close();
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> inParams.SD_y;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'inParams.SD_y' assigned to ";
    outfile << inParams.SD_y;
    outfile << " in module InputFormat" << std::endl;
    outfile.close();
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> inParams.SD_z;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'inParams.SD_z' assigned to ";
    outfile << inParams.SD_z;
    outfile << " in module InputFormat" << std::endl;
    outfile.close();
    infile.close();
}

