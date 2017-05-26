#include "parameters.h"

#ifndef PCM_ERROR_H_INCLUDED
#define PCM_ERROR_H_INCLUDED

double PCM_ErrorF(const char Ffile[], const char Cfile[], const char comparator[], struct parameters params);
double PCM_ErrorM(const char Mfile[], const char Cfile[], const char comparator[], struct parameters params);
double PCM_ErrorC(const char Cfile1[], const char Cfile2[], const char comparator[]);
double errorCalcInterp(double time1[], double time2[], double result1[], double result2[], int sizeOfResults1, int sizeOfResults2);

#endif // PCM_ERROR_H_INCLUDED
