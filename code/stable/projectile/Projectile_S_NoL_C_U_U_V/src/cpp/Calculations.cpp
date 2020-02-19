#include "Calculations.hpp"

#include <math.h>
#include <string>

using std::string;

double func_t_flight(double v_launch, double theta, double g_vect) {
    return 2 * v_launch * sin(theta) / g_vect;
}

double func_p_land(double v_launch, double theta, double g_vect) {
    return 2 * pow(v_launch, 2) * sin(theta) * cos(theta) / g_vect;
}

double func_d_offset(double p_target, double p_land) {
    return p_land - p_target;
}

string func_s(double p_target, double epsilon, double d_offset) {
    if (fabs(d_offset / p_target) < epsilon) {
        return "The target was hit.";
    }
    else if (d_offset < 0) {
        return "The projectile fell short.";
    }
    else {
        return "The projectile went long.";
    }
}
