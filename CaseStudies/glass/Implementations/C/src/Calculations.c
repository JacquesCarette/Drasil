#include "Calculations.h"

#include <math.h>
#include <stdlib.h>
#include <stdio.h>

#include "InputParameters.h"
#include "Constants.h"
#include "Interpolation.h"
#include "ArrayLib.h"

double calc_q(double* w_array, int w_array_n, double** data_sd, int data_sd_n, double** data_q, int data_q_n, InputParameters* params) {
    int idx = find_idx(w_array, w_array_n, params->wtnt);
    int jdx = find_jdx(data_sd, data_sd_n, params->sd, idx);
    int kdx = find_kdx(data_sd, data_sd_n, params->sd, idx);
    int num_interp1 = find_num_interp1(w_array, w_array_n, params->wtnt, idx);
    int num_interp2 = find_num_interp2(w_array, w_array_n, data_sd, data_sd_n, params->wtnt, params->sd, idx);
    double q = interp(idx, jdx, kdx, num_interp1, num_interp2, w_array, data_sd, data_q, params->wtnt, params->sd);
    return q;
}

int calc_q_vec(double* j_array, int j_array_n, double** data_asprat, int data_asprat_n, double** data_qstar, InputParameters* params, double** q_vec) {
    *q_vec = (double*)malloc(sizeof(double) * j_array_n);
    int q_vec_n = 0;
    for (int i = 0; i < j_array_n; i++) {
        int idx_2 = i;
        int jdx_2 = arrayArgMin(double)(arrayAbs(double)(arraySubt(double)(matrixCol(double)(data_asprat, data_asprat_n, idx_2), data_asprat_n, params->asprat), data_asprat_n), data_asprat_n);
        if (data_asprat[jdx_2][idx_2] > params->asprat)
            jdx_2 -= 1;
        if (data_asprat[0][idx_2] > params->asprat)
            continue;
        else {
            double q_star = interp(idx_2, jdx_2, -1, 0, 1, j_array, data_asprat, data_qstar, -1, params->asprat);
            *(*q_vec + q_vec_n) = q_star;
            ++q_vec_n;
        }
    }
    return q_vec_n;
}

int calc_j_vec(double* j_array, int j_array_n, double** data_asprat, int data_asprat_n, double** data_qstar, InputParameters* params, double** j_vec) {
    *j_vec = (double*)malloc(sizeof(double) * j_array_n);
    int j_vec_n = 0;
    for (int i = 0; i < j_array_n; i++) {
        int idx_2 = i;
        int jdx_2 = arrayArgMin(double)(arrayAbs(double)(arraySubt(double)(matrixCol(double)(data_asprat, data_asprat_n, idx_2), data_asprat_n, params->asprat), data_asprat_n), data_asprat_n);
        if (data_asprat[jdx_2][idx_2] > params->asprat)
            jdx_2 -= 1;
        if (data_asprat[0][idx_2] > params->asprat)
            continue;
        else {
            *(*j_vec + j_vec_n) = j_array[idx_2];
            ++j_vec_n;
        }
    }
    return j_vec_n;
}

double calc_q_hat(double q, double* q_vec, int q_vec_n, InputParameters* params) {
    double q_hat = q * pow((params->a * params->b), 2.0) / (E * pow(params->h, 4.0)) * (1.0 / params->gtf);
    if (q_hat < arrayMin(double)(q_vec, q_vec_n)) {
        printf("Error: q_hat(DD6 in the SRS) is out of bounds. q_hat is less than the smallest value in q_vec. The input a, b might be too small while t might be too large. Please refer to the data definitions section and the data constraints section in the SRS.");
        exit(EXIT_FAILURE);
    }
    if (q_hat > arrayMax(double)(q_vec, q_vec_n)) {
        printf("Error: q_hat(DD6 in SRS) is out of bounds. q_hat is greater than the biggest value in q_vec. The input a, b might be too large while t might be too small. Please refer to the data definitions section and the data constraints section in the SRS.");
        exit(EXIT_FAILURE);
    }
    return q_hat;
}
    
double calc_j_tol(InputParameters* params) {
    double j_tol = log((log(1.0/(1.0-params->pbtol)))*((pow((params->a/1000.0)*(params->b/1000.0), M-1.0))/(K *
                                                (pow(E*1000.0*(pow((params->h/1000.0), 2.0)), M))*params->ldf)));
    return j_tol;                                   
} 

double calc_j(double* q_vec, int q_vec_n, double* j_vec, double q_hat) {
    int idx_3 = arrayArgMin(double)(arrayAbs(double)(arraySubt(double)(q_vec, q_vec_n, q_hat), q_vec_n), q_vec_n);
    if (q_vec[idx_3] > q_hat)
        idx_3 -= 1;
    double j = lin_interp(j_vec[idx_3], j_vec[idx_3+1], q_vec[idx_3], q_vec[idx_3+1], q_hat);
    return j;
}
    
double calc_q_hat_tol(double* q_vec, double* j_vec, int j_vec_n, double j_tol) {
    int idx_4 = arrayArgMin(double)(arrayAbs(double)(arraySubt(double)(j_vec, j_vec_n, j_tol), j_vec_n), j_vec_n);
    if (j_vec[idx_4] > j_tol)
        idx_4 -= 1;
    double q_hat_tol = lin_interp(q_vec[idx_4], q_vec[idx_4+1], j_vec[idx_4], j_vec[idx_4+1], j_tol);
    return q_hat_tol;
}

double calc_pb(double j, InputParameters* params) {
    double b = (K / (pow(params->a/1000.0 * params->b/1000.0, M - 1.0))) * (
        pow(1000.0* E * pow(params->h/1000.0, 2.0), M)) * params->ldf * (exp(j));
    double pb = 1.0 - exp(-b);
    return pb;
}

double calc_nfl(double q_hat_tol, InputParameters* params) {
    double nfl = (q_hat_tol * E * pow(params->h, 4.0)) / (pow(params->a * params->b, 2.0));
    return nfl;
}
    
double calc_lr(double nfl, InputParameters* params) {
    double lr = nfl * params->gtf * LSF;
    return lr;
}

int calc_is_safe1(double pb, InputParameters* params) {
    int is_safe1;
    if (pb < params->pbtol)
        is_safe1 = 1;
    else
        is_safe1 = 0;
    return is_safe1;
}
    
int calc_is_safe2(double lr, double q) {
    int is_safe2;
    if (lr > q)
        is_safe2 = 1;
    else
        is_safe2 = 0;
    return is_safe2;
}