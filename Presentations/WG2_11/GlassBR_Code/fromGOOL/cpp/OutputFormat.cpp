#include "OutputFormat.hpp"

#include "InputParameters.hpp"

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

void GlassBR::display_output(string filename, double q, double j, double q_hat_tol, double pb, double lr, double nfl, bool is_safe1, bool is_safe2, InputParameters &inparams) {
    ofstream outfile;
    outfile.open(filename, std::fstream::out | std::fstream::app);
    outfile << "a           ";
    outfile << inparams.a << std::endl;
    outfile << "b           ";
    outfile << inparams.b << std::endl;
    outfile << "t           ";
    outfile << inparams.t << std::endl;
    outfile << "w           ";
    outfile << inparams.w << std::endl;
    outfile << "tnt         ";
    outfile << inparams.tnt << std::endl;
    outfile << "sdx         ";
    outfile << inparams.sdx << std::endl;
    outfile << "sdy         ";
    outfile << inparams.sdy << std::endl;
    outfile << "sdz         ";
    outfile << inparams.sdz << std::endl;
    outfile << "pbtol       ";
    outfile << inparams.pbtol << std::endl;
    outfile << "asprat      ";
    outfile << inparams.asprat << std::endl;
    outfile << "sd          ";
    outfile << inparams.sd << std::endl;
    outfile << "h           ";
    outfile << inparams.h << std::endl;
    outfile << "gtf         ";
    outfile << inparams.gtf << std::endl;
    outfile << "ldf         ";
    outfile << inparams.ldf << std::endl;
    outfile << "wtnt        ";
    outfile << inparams.wtnt << std::endl;
    outfile << "E           ";
    outfile << inparams.E << std::endl;
    outfile << "td          ";
    outfile << inparams.td << std::endl;
    outfile << "m           ";
    outfile << inparams.m << std::endl;
    outfile << "k           ";
    outfile << inparams.k << std::endl;
    outfile << "lsf         ";
    outfile << inparams.lsf << std::endl;
    outfile << "gt          ";
    outfile << inparams.gt << std::endl;
    outfile << "Demand (q)                      ";
    outfile << q << std::endl;
    outfile << "Stress Distr. Factor (j)        ";
    outfile << j << std::endl;
    outfile << "Tolerable Pressure (q_hat_tol)  ";
    outfile << q_hat_tol << std::endl;
    outfile << "Prob. of Breakage (pb)          ";
    outfile << pb << std::endl;
    outfile << "Capacity (lr)                   ";
    outfile << lr << std::endl;
    outfile << "Non-Factored Load (nfl)         ";
    outfile << nfl << std::endl;
    outfile << "Safety Req. 1 (is_safe1)        ";
    outfile << is_safe1 << std::endl;
    outfile << "Safety Req. 2 (is_safe2)        ";
    outfile << is_safe2 << std::endl;
    if (is_safe1 && is_safe2) {
        outfile << "For the given input parameters, the glass is considered safe." << std::endl;
    }
    else {
        outfile << "For the given input parameters, the glass is NOT considered safe." << std::endl;
    }
    outfile.close();
}

