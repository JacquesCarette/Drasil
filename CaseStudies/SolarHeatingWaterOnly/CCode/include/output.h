#include "parameters.h"

#ifndef OUTPUT_H_INCLUDED
#define OUTPUT_H_INCLUDED

void output(char* filename, double time[], double tempWat[], double tempPCM[], double eWat[], double ePCM[], double eTot[], struct parameters params, int sizeOfResults);

#endif // OUTPUT_H_INCLUDED
