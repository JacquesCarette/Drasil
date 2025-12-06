#include "InputParameters.hpp"

#include <algorithm>
#include <fstream>
#include <iostream>
#include <limits>
#include <math.h>
#include <set>
#include <string>

using std::ifstream;
using std::ofstream;
using std::set;
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
    this->derived_values();
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
    infile >> this->a;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'this->a' assigned ";
    outfile << this->a;
    outfile << " in module InputParameters" << std::endl;
    outfile.close();
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> this->b;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'this->b' assigned ";
    outfile << this->b;
    outfile << " in module InputParameters" << std::endl;
    outfile.close();
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> this->w;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'this->w' assigned ";
    outfile << this->w;
    outfile << " in module InputParameters" << std::endl;
    outfile.close();
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> this->P_btol;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'this->P_btol' assigned ";
    outfile << this->P_btol;
    outfile << " in module InputParameters" << std::endl;
    outfile.close();
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> this->TNT;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'this->TNT' assigned ";
    outfile << this->TNT;
    outfile << " in module InputParameters" << std::endl;
    outfile.close();
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> this->g;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'this->g' assigned ";
    outfile << this->g;
    outfile << " in module InputParameters" << std::endl;
    outfile.close();
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> this->t;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'this->t' assigned ";
    outfile << this->t;
    outfile << " in module InputParameters" << std::endl;
    outfile.close();
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> this->SD_x;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'this->SD_x' assigned ";
    outfile << this->SD_x;
    outfile << " in module InputParameters" << std::endl;
    outfile.close();
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> this->SD_y;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'this->SD_y' assigned ";
    outfile << this->SD_y;
    outfile << " in module InputParameters" << std::endl;
    outfile.close();
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> this->SD_z;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'this->SD_z' assigned ";
    outfile << this->SD_z;
    outfile << " in module InputParameters" << std::endl;
    outfile.close();
    infile.close();
}

void InputParameters::derived_values() {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function derived_values called with inputs: {" << std::endl;
    outfile << "  }" << std::endl;
    outfile.close();
    
    this->h = 1.0 / 1000.0 * (this->t == 2.5 ? 2.16 : this->t == 2.7 ? 2.59 : this->t == 3.0 ? 2.92 : this->t == 4.0 ? 3.78 : this->t == 5.0 ? 4.57 : this->t == 6.0 ? 5.56 : this->t == 8.0 ? 7.42 : this->t == 10.0 ? 9.02 : this->t == 12.0 ? 11.91 : this->t == 16.0 ? 15.09 : this->t == 19.0 ? 18.26 : 21.44);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'this->h' assigned ";
    outfile << this->h;
    outfile << " in module InputParameters" << std::endl;
    outfile.close();
    
    this->LDF = pow(3.0 / 60.0, 7.0 / 16.0);
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'this->LDF' assigned ";
    outfile << this->LDF;
    outfile << " in module InputParameters" << std::endl;
    outfile.close();
    
    if (this->g == "AN") {
        this->GTF = 1;
        outfile.open("log.txt", std::fstream::app);
        outfile << "var 'this->GTF' assigned ";
        outfile << this->GTF;
        outfile << " in module InputParameters" << std::endl;
        outfile.close();
    }
    else if (this->g == "FT") {
        this->GTF = 4;
        outfile.open("log.txt", std::fstream::app);
        outfile << "var 'this->GTF' assigned ";
        outfile << this->GTF;
        outfile << " in module InputParameters" << std::endl;
        outfile.close();
    }
    else if (this->g == "HS") {
        this->GTF = 2;
        outfile.open("log.txt", std::fstream::app);
        outfile << "var 'this->GTF' assigned ";
        outfile << this->GTF;
        outfile << " in module InputParameters" << std::endl;
        outfile.close();
    }
    else {
        throw("Undefined case encountered in function GTF");
    }
    
    this->SD = sqrt(pow(this->SD_x, 2.0) + pow(this->SD_y, 2.0) + pow(this->SD_z, 2.0));
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'this->SD' assigned ";
    outfile << this->SD;
    outfile << " in module InputParameters" << std::endl;
    outfile.close();
    
    this->AR = this->a / this->b;
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'this->AR' assigned ";
    outfile << this->AR;
    outfile << " in module InputParameters" << std::endl;
    outfile.close();
    
    this->w_TNT = this->w * this->TNT;
    outfile.open("log.txt", std::fstream::app);
    outfile << "var 'this->w_TNT' assigned ";
    outfile << this->w_TNT;
    outfile << " in module InputParameters" << std::endl;
    outfile.close();
}

void InputParameters::input_constraints() {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function input_constraints called with inputs: {" << std::endl;
    outfile << "  }" << std::endl;
    outfile.close();
    
    if (!(0.1 <= this->a && this->a <= 5.0)) {
        std::cout << "a has value ";
        std::cout << this->a;
        std::cout << ", but is expected to be ";
        std::cout << "between ";
        std::cout << 0.1;
        std::cout << " and ";
        std::cout << 5.0;
        std::cout << "." << std::endl;
        throw("InputError");
    }
    if (!(0.1 <= this->b && this->b <= 5.0)) {
        std::cout << "b has value ";
        std::cout << this->b;
        std::cout << ", but is expected to be ";
        std::cout << "between ";
        std::cout << 0.1;
        std::cout << " and ";
        std::cout << 5.0;
        std::cout << "." << std::endl;
        throw("InputError");
    }
    if (!(4.5 <= this->w && this->w <= 910.0)) {
        std::cout << "w has value ";
        std::cout << this->w;
        std::cout << ", but is expected to be ";
        std::cout << "between ";
        std::cout << 4.5;
        std::cout << " and ";
        std::cout << 910.0;
        std::cout << "." << std::endl;
        throw("InputError");
    }
    set<string> set_g = {"AN", "FT", "HS"};
    if (!(set_g.find(this->g) != set_g.end())) {
        std::cout << "g has value ";
        std::cout << this->g;
        std::cout << ", but is expected to be ";
        std::cout << "an element of the set ";
        std::cout << "{ ";
        for (const string &set_i1 : set_g) {
            std::cout << set_i1;
            std::cout << " ";
        }
        std::cout << "}";
        std::cout << "." << std::endl;
        throw("InputError");
    }
    set<double> set_t = {2.5, 2.7, 3.0, 4.0, 5.0, 6.0, 8.0, 10.0, 12.0, 16.0, 19.0, 22.0};
    if (!(set_t.find(this->t) != set_t.end())) {
        std::cout << "t has value ";
        std::cout << this->t;
        std::cout << ", but is expected to be ";
        std::cout << "an element of the set ";
        std::cout << "{ ";
        for (const double &set_i1 : set_t) {
            std::cout << set_i1;
            std::cout << " ";
        }
        std::cout << "}";
        std::cout << "." << std::endl;
        throw("InputError");
    }
    if (!(6.0 <= this->SD && this->SD <= 130.0)) {
        std::cout << "SD has value ";
        std::cout << this->SD;
        std::cout << ", but is expected to be ";
        std::cout << "between ";
        std::cout << 6.0;
        std::cout << " and ";
        std::cout << 130.0;
        std::cout << "." << std::endl;
        throw("InputError");
    }
    if (!(this->AR <= 5.0)) {
        std::cout << "AR has value ";
        std::cout << this->AR;
        std::cout << ", but is expected to be ";
        std::cout << "below ";
        std::cout << 5.0;
        std::cout << "." << std::endl;
        throw("InputError");
    }
    
    if (!(this->a > 0.0)) {
        std::cout << "a has value ";
        std::cout << this->a;
        std::cout << ", but is expected to be ";
        std::cout << "above ";
        std::cout << 0.0;
        std::cout << "." << std::endl;
        throw("InputError");
    }
    if (!(this->a >= this->b)) {
        std::cout << "a has value ";
        std::cout << this->a;
        std::cout << ", but is expected to be ";
        std::cout << "above ";
        std::cout << this->b;
        std::cout << "." << std::endl;
        throw("InputError");
    }
    if (!(0.0 < this->b && this->b <= this->a)) {
        std::cout << "b has value ";
        std::cout << this->b;
        std::cout << ", but is expected to be ";
        std::cout << "between ";
        std::cout << 0.0;
        std::cout << " and ";
        std::cout << this->a;
        std::cout << "." << std::endl;
        throw("InputError");
    }
    if (!(this->w > 0.0)) {
        std::cout << "w has value ";
        std::cout << this->w;
        std::cout << ", but is expected to be ";
        std::cout << "above ";
        std::cout << 0.0;
        std::cout << "." << std::endl;
        throw("InputError");
    }
    if (!(0.0 <= this->P_btol && this->P_btol <= 1.0)) {
        std::cout << "P_btol has value ";
        std::cout << this->P_btol;
        std::cout << ", but is expected to be ";
        std::cout << "between ";
        std::cout << 0.0;
        std::cout << " and ";
        std::cout << 1.0;
        std::cout << "." << std::endl;
        throw("InputError");
    }
    if (!(this->TNT > 0.0)) {
        std::cout << "TNT has value ";
        std::cout << this->TNT;
        std::cout << ", but is expected to be ";
        std::cout << "above ";
        std::cout << 0.0;
        std::cout << "." << std::endl;
        throw("InputError");
    }
    if (!(this->SD > 0.0)) {
        std::cout << "SD has value ";
        std::cout << this->SD;
        std::cout << ", but is expected to be ";
        std::cout << "above ";
        std::cout << 0.0;
        std::cout << "." << std::endl;
        throw("InputError");
    }
    if (!(this->AR >= 1.0)) {
        std::cout << "AR has value ";
        std::cout << this->AR;
        std::cout << ", but is expected to be ";
        std::cout << "above ";
        std::cout << 1.0;
        std::cout << "." << std::endl;
        throw("InputError");
    }
}
