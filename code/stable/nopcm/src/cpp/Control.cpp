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
#include "OutputFormat.hpp"
#include "InputFormat.hpp"

int main(int argc, const char *argv[]) {
    string inputfile = argv[1];
    InputParameters inParams = InputParameters();
    func_get_input(inputfile, inParams);
    input_constraints(A_C, C_W, h_C, T_init, t_final, L, T_C, t_step, rho_W, D, T_W, E_W);
    write_output(T_W, E_W);
    return 0;
}

