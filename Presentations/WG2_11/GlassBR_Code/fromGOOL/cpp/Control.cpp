
#include "InputParameters.hpp"
#include "InputFormat.hpp"
#include "DerivedValues.hpp"
#include "InputConstraints.hpp"
#include "Interpolation.hpp"
#include "Calculations.hpp"
#include "OutputFormat.hpp"
#include "ReadTable.hpp"

#include <algorithm>
#include <iostream>
#include <fstream>
#include <iterator>
#include <string>
#include <math.h>
#include <sstream>
#include <vector>

using namespace GlassBR;
using std::string;
using std::vector;
using std::ifstream;
using std::ofstream;

int main(int argc, const char *argv[]) {
    string filename = argv[1];
    InputParameters inparams = InputParameters();
    get_input(filename, inparams);
    derived_params(inparams);
    check_constraints(inparams);
    vector<double> w_array = read_z_array("TSD.txt");
    vector< vector<double> > data_sd = read_x_array("TSD.txt");
    vector< vector<double> > data_q = read_y_array("TSD.txt");
    vector<double> j_array = read_z_array("SDF.txt");
    vector< vector<double> > data_asprat = read_x_array("SDF.txt");
    vector< vector<double> > data_qstar = read_y_array("SDF.txt");
    double q = interpY(data_sd, data_q, w_array, inparams.sd, inparams.wtnt);
    double q_hat = calc_q_hat(q, inparams);
    double j_tol = calc_j_tol(inparams);
    double j = interpZ(data_asprat, data_qstar, j_array, inparams.asprat, q_hat);
    double q_hat_tol = interpY(data_asprat, data_qstar, j_array, inparams.asprat, j_tol);
    double pb = calc_pb(j, inparams);
    double nfl = calc_nfl(q_hat_tol, inparams);
    double lr = calc_lr(nfl, inparams);
    bool is_safe1 = calc_is_safe1(pb, inparams);
    bool is_safe2 = calc_is_safe2(lr, q);
    display_output("outputfile.txt", q, j, q_hat_tol, pb, lr, nfl, is_safe1, is_safe2, inparams);
    std::cout << "Main has been executed and the results have been written to 'outputfile.txt'." << std::endl;
    
    return 0;
}

