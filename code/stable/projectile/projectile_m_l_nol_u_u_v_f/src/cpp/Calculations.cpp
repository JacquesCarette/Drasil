#include "Calculations.hpp"

#include <math.h>

float func_t_flight(float v_launch, float theta, float g) {
    return 2.0f * v_launch * sin(theta) / g;
}

float func_p_land(float v_launch, float theta, float g) {
    return 2.0f * pow(v_launch, 2.0f) * sin(theta) * cos(theta) / g;
}

float func_d_offset(float p_target, float p_land) {
    return p_land - p_target;
}
