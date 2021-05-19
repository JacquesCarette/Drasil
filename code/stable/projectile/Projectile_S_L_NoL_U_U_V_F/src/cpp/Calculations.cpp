#include "Calculations.hpp"

#include <math.h>
#include <string>

using std::string;

float func_t_flight(float v_launch, float theta, float g_vect) {
    return 2.0f * v_launch * sin(theta) / g_vect;
}

float func_p_land(float v_launch, float theta, float g_vect) {
    return 2.0f * pow(v_launch, 2) * sin(theta) * cos(theta) / g_vect;
}

float func_d_offset(float p_target, float p_land) {
    return p_land - p_target;
}

string func_s(float p_target, float epsilon, float d_offset) {
    if (fabs(d_offset / p_target) < epsilon) {
        return "The target was hit.";
    }
    else if (d_offset < 0.0f) {
        return "The projectile fell short.";
    }
    else {
        return "The projectile went long.";
    }
}
