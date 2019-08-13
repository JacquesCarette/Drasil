#include "InputConstraints.hpp"

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

void input_constraints(InputParameters &inParams) {
    ofstream outfile;
    outfile.open("log.txt", std::fstream::app);
    outfile << "function input_constraints called with inputs: {" << std::endl;
    outfile << "  inParams = ";
    outfile << "Instance of InputParameters object" << std::endl;
    outfile << "  }" << std::endl;
    outfile.close();
    
    if (!(0.1 <= inParams.a && inParams.a <= 5.0)) {
        std::cout << "a has value ";
        std::cout << inParams.a;
        std::cout << " but expected to be ";
        std::cout << "between ";
        std::cout << 0.1;
        std::cout << " (d_min)";
        std::cout << " and ";
        std::cout << 5.0;
        std::cout << " (d_max)";
        std::cout << "." << std::endl;
        throw("InputError");
    }
    if (!(0.1 <= inParams.b && inParams.b <= 5.0)) {
        std::cout << "b has value ";
        std::cout << inParams.b;
        std::cout << " but expected to be ";
        std::cout << "between ";
        std::cout << 0.1;
        std::cout << " (d_min)";
        std::cout << " and ";
        std::cout << 5.0;
        std::cout << " (d_max)";
        std::cout << "." << std::endl;
        throw("InputError");
    }
    if (!(4.5 <= inParams.w && inParams.w <= 910.0)) {
        std::cout << "w has value ";
        std::cout << inParams.w;
        std::cout << " but expected to be ";
        std::cout << "between ";
        std::cout << 4.5;
        std::cout << " (w_min)";
        std::cout << " and ";
        std::cout << 910.0;
        std::cout << " (w_max)";
        std::cout << "." << std::endl;
        throw("InputError");
    }
    if (!(6.0 <= inParams.SD && inParams.SD <= 130.0)) {
        std::cout << "SD has value ";
        std::cout << inParams.SD;
        std::cout << " but expected to be ";
        std::cout << "between ";
        std::cout << 6.0;
        std::cout << " (SD_min)";
        std::cout << " and ";
        std::cout << 130.0;
        std::cout << " (SD_max)";
        std::cout << "." << std::endl;
        throw("InputError");
    }
    if (!(inParams.AR <= 5.0)) {
        std::cout << "AR has value ";
        std::cout << inParams.AR;
        std::cout << " but expected to be ";
        std::cout << "below ";
        std::cout << 5.0;
        std::cout << " (AR_max)";
        std::cout << "." << std::endl;
        throw("InputError");
    }
    
    if (!(inParams.a > 0)) {
        std::cout << "a has value ";
        std::cout << inParams.a;
        std::cout << " but expected to be ";
        std::cout << "above ";
        std::cout << 0;
        std::cout << "." << std::endl;
        throw("InputError");
    }
    if (!(inParams.a >= inParams.b)) {
        std::cout << "a has value ";
        std::cout << inParams.a;
        std::cout << " but expected to be ";
        std::cout << "above ";
        std::cout << inParams.b;
        std::cout << " (b)";
        std::cout << "." << std::endl;
        throw("InputError");
    }
    if (!(0 < inParams.b && inParams.b <= inParams.a)) {
        std::cout << "b has value ";
        std::cout << inParams.b;
        std::cout << " but expected to be ";
        std::cout << "between ";
        std::cout << 0;
        std::cout << " and ";
        std::cout << inParams.a;
        std::cout << " (a)";
        std::cout << "." << std::endl;
        throw("InputError");
    }
    if (!(inParams.w > 0)) {
        std::cout << "w has value ";
        std::cout << inParams.w;
        std::cout << " but expected to be ";
        std::cout << "above ";
        std::cout << 0;
        std::cout << "." << std::endl;
        throw("InputError");
    }
    if (!(0 < inParams.P_btol && inParams.P_btol < 1)) {
        std::cout << "P_btol has value ";
        std::cout << inParams.P_btol;
        std::cout << " but expected to be ";
        std::cout << "between ";
        std::cout << 0;
        std::cout << " and ";
        std::cout << 1;
        std::cout << "." << std::endl;
        throw("InputError");
    }
    if (!(inParams.TNT > 0)) {
        std::cout << "TNT has value ";
        std::cout << inParams.TNT;
        std::cout << " but expected to be ";
        std::cout << "above ";
        std::cout << 0;
        std::cout << "." << std::endl;
        throw("InputError");
    }
    if (!(inParams.SD > 0)) {
        std::cout << "SD has value ";
        std::cout << inParams.SD;
        std::cout << " but expected to be ";
        std::cout << "above ";
        std::cout << 0;
        std::cout << "." << std::endl;
        throw("InputError");
    }
    if (!(inParams.AR >= 1)) {
        std::cout << "AR has value ";
        std::cout << inParams.AR;
        std::cout << " but expected to be ";
        std::cout << "above ";
        std::cout << 1;
        std::cout << "." << std::endl;
        throw("InputError");
    }
}

