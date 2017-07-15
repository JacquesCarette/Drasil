#include "Calculations.hpp"

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

double GlassBR::calc_q_hat(double q, InputParameters &inparams) {
    double q_hat = ((q * (pow(inparams.a * inparams.b, 2.0))) / (inparams.E * (pow(inparams.h, 4.0)))) * (1.0 / inparams.gtf);
    return q_hat;
}

double GlassBR::calc_j_tol(InputParameters &inparams) {
    double j_tol = log((log(1.0 / (1.0 - inparams.pbtol))) * ((pow((inparams.a / 1000.0) * (inparams.b / 1000.0), inparams.m - 1.0)) / ((inparams.k * (pow((inparams.E * 1000.0) * (pow(inparams.h / 1000.0, 2.0)), inparams.m))) * inparams.ldf)));
    return j_tol;
}

double GlassBR::calc_pb(double j, InputParameters &inparams) {
    double b = (((inparams.k / (pow(((inparams.a / 1000.0) * inparams.b) / 1000.0, inparams.m - 1.0))) * (pow((1000.0 * inparams.E) * (pow(inparams.h / 1000.0, 2.0)), inparams.m))) * inparams.ldf) * (exp(j));
    double pb = 1.0 - (exp(-(b)));
    return pb;
}

double GlassBR::calc_nfl(double q_hat_tol, InputParameters &inparams) {
    double nfl = ((q_hat_tol * inparams.E) * (pow(inparams.h, 4.0))) / (pow(inparams.a * inparams.b, 2.0));
    return nfl;
}

double GlassBR::calc_lr(double nfl, InputParameters &inparams) {
    double lr = (nfl * inparams.gtf) * inparams.lsf;
    return lr;
}

bool GlassBR::calc_is_safe1(double pb, InputParameters &inparams) {
    bool is_safe1;
    if (pb < inparams.pbtol) {
        is_safe1 = true;
    }
    else {
        is_safe1 = false;
    }
    return is_safe1;
}

bool GlassBR::calc_is_safe2(double lr, double q) {
    bool is_safe2;
    if (lr > q) {
        is_safe2 = true;
    }
    else {
        is_safe2 = false;
    }
    return is_safe2;
}

