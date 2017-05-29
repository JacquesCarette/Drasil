#ifndef CALCULATIONS_H
#define CALCULATIONS_H

#include "InputParameters.h"

double calc_q(double* w_array, int w_array_n, double** data_sd, int data_sd_n, double** data_q, int data_q_n, InputParameters* params);
int calc_q_vec(double* j_array, int j_array_n, double** data_asprat, int data_asprat_n, double** data_qstar, InputParameters* params, double** q_vec);
int calc_j_vec(double* j_array, int j_array_n, double** data_asprat, int data_asprat_n, double** data_qstar, InputParameters* params, double** j_vec);
double calc_q_hat(double q, double* q_vec, int q_vec_n, InputParameters* params);
double calc_j_tol(InputParameters* params);
double calc_j(double* q_vec, int q_vec_n, double* j_vec, double q_hat);
double calc_q_hat_tol(double* q_vec, double* j_vec, int j_vec_n, double j_tol);
double calc_pb(double j, InputParameters* params);
double calc_nfl(double q_hat_tol, InputParameters* params); 
double calc_lr(double nfl, InputParameters* params);
int calc_is_safe1(double pb, InputParameters* params);
int calc_is_safe2(double lr, double q);

#endif /* CALCULATIONS_H */