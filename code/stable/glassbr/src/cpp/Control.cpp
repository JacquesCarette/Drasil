
#include "InputParameters.hpp"
#include "DerivedValues.hpp"
#include "InputConstraints.hpp"
#include "InputFormat.hpp"
#include "OutputFormat.hpp"
#include "Calculations.hpp"

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

int main(int argc, const char *argv[]) {
    string inputfile = argv[1];
    InputParameters inParams = InputParameters();
    func_get_input(inputfile, inParams);
    derived_values(inParams);
    input_constraints(inParams);
    double q = func_q(inParams);
    double J_tol = func_J_tol(inParams);
    double q = func_q(inParams);
    double q_hat = func_q_hat(inParams, q);
    double q_hat_tol = func_q_hat_tol(inParams, J_tol);
    double J = func_J(inParams, q_hat);
    double NFL = func_NFL(inParams, q_hat_tol);
    double B = func_B(inParams, J);
    double LR = func_LR(inParams, NFL);
    bool is_safeLR = func_is_safeLR(LR, q);
    double P_b = func_P_b(B);
    bool is_safePb = func_is_safePb(inParams, P_b);
    write_output(is_safePb, is_safeLR, P_b);
    
    return 0;
}

