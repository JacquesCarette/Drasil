#include <stdio.h>
#include <stdlib.h>

#include "calc.h"

int err = 0;

double calc_h_g(double k_c, double h_p, double tau_c){
    double numerator = 2.0 * k_c * h_p;
    double denominator = 2.0 * k_c + tau_c * h_p;
    if (denominator == 0){
        err = 1;
        return -1.0;
    }
    err = 0;
    return numerator/denominator;
}

double calc_h_c(double k_c, double h_b, double tau_c){
    double numerator = 2.0 * k_c * h_b;
    double denominator = 2.0 * k_c + tau_c * h_b;
    if (denominator == 0){
        err = 1;
        return -1.0;
    }
    err = 0;
    return numerator/denominator;
}

int calc_get_error_code(void){
    return err;
}
