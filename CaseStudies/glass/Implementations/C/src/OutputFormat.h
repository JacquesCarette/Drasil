#ifndef OUTPUTFORMAT_H
#define OUTPUTFORMAT_H

#include "InputParameters.h"

void display_output(char* filename, double q, double j, double q_hat_tol, double pb, double lr, double nfl, int is_safe1, int is_safe2, InputParameters* params);

#endif /* OUTPUTFORMAT_H */