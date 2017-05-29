#ifndef INPUTFORMAT_H
#define INPUTFORMAT_H

#include "InputParameters.h"

void get_input(char* filename, InputParameters* params);
double* read_table_header(char* filename, int start, int stop, int step, char delim);

#endif /* INPUTFORMAT_H */