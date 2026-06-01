#include "Calculations.hpp"

#include <math.h>

#include "InputParameters.hpp"

double func_t_flight(InputParameters &inParams, double g) {
    return 2.0 * inParams.v_launch * sin(inParams.theta) / g;
}

double func_p_land(InputParameters &inParams, double g) {
    return 2.0 * pow(inParams.v_launch, 2.0) * sin(inParams.theta) * cos(inParams.theta) / g;
}

double func_d_offset(InputParameters &inParams, double p_land) {
    return p_land - inParams.p_target;
}
