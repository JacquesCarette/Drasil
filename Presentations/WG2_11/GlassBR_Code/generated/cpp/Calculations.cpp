#include "Calculations.hpp"


#include <algorithm>
#include <iostream>
#include <fstream>
#include <iterator>
#include <string>
#include <math.h>
#include <sstream>
#include <vector>

using namespace GlassBR_program;
using std::string;
using std::vector;
using std::ifstream;
using std::ofstream;

double GlassBR_program::calc_B(double k, double a, double b, double m, double E, double h, double LDF, double J) {
    return (((k / (pow((a / 1000) * (b / 1000), m - 1))) * (pow((E * 1000) * (pow(h / 1000, 2)), m))) * LDF) * (exp(J));
}

double GlassBR_program::calc_h(double t) {
    if (t == 2.5) {
        return 2.16;
    }
    else if (t == 2.7) {
        return 2.59;
    }
    else if (t == 3.0) {
        return 2.92;
    }
    else if (t == 4.0) {
        return 3.78;
    }
    else if (t == 5.0) {
        return 4.57;
    }
    else if (t == 6.0) {
        return 5.56;
    }
    else if (t == 8.0) {
        return 7.42;
    }
    else if (t == 10.0) {
        return 9.02;
    }
    else if (t == 12.0) {
        return 11.91;
    }
    else if (t == 16.0) {
        return 15.09;
    }
    else if (t == 19.0) {
        return 18.26;
    }
    else if (t == 22.0) {
        return 21.44;
    }
}

double GlassBR_program::calc_LDF(double t_d, double m) {
    return pow(t_d / 60, m / 16);
}

double GlassBR_program::calc_J(double J, double q_hat, double a, double b) {
    return J(q_hat, a / b);
}

double GlassBR_program::calc_NFL(double q_hat_tol, double E, double h, double a, double b) {
    return ((q_hat_tol * E) * (pow(h, 4))) / (pow(a * b, 2));
}

double GlassBR_program::calc_GTF(double g) {
    if (g == "AN") {
        return 1;
    }
    else if (g == "FT") {
        return 4;
    }
    else if (g == "HS") {
        return 2;
    }
}

double GlassBR_program::calc_q_hat(double q, double a, double b, double E, double h, double GTF) {
    return (q * (pow(a * b, 2))) / ((E * (pow(h, 4))) * GTF);
}

double GlassBR_program::calc_q_hat_tol(double q_hat_tol, double J_tol, double a, double b) {
    return q_hat_tol(J_tol, a / b);
}

double GlassBR_program::calc_J_tol(double P_btol, double a, double b, double m, double k, double E, double h, double LDF) {
    return log((log(1 / (1 - P_btol))) * ((pow((a / 1000) * (b / 1000), m - 1)) / ((k * (pow((E * 1000) * (pow(h / 1000, 2)), m))) * LDF)));
}

