// This function performs simple linear interpolation given two points in a data set and the desired x-coordiante
#include "linterp.h"


double linterp(double time1, double time2, double result1, double result2, double desiredTime){

    double desiredResult;

    desiredResult = result1 + ((desiredTime - time1) / (time2 - time1)) * (result2 - result1);

    return desiredResult;
}
