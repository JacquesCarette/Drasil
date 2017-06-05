#include "InputParameters.h"
#include "InputFormat.h"
#include "DerivedValues.h"
#include "InputConstraints.h"
#include "ReadTable.h"
#include "Calculations.h"
#include "ArrayLib.h"
#include "Constants.h"
#include "OutputFormat.h"

#include <stdio.h>

int main(int argc, char* argv[]){
  InputParameters* params = new_InputParameters();
  get_input(argv[1], params);
  derived_params(params);
  check_constraints(params);
  
  double* w_array;
  int w_array_n = read_table_row(double)("TSD.txt", 0, 1, 0, 2, ',', &w_array);
  
  double** data_sd;
  double** data_q;
  int data_sd_n = read_table(double)("TSD.txt", 1, 0, w_array_n*2, 2, ',', &data_sd);
  int data_q_n = read_table(double)("TSD.txt", 1, 1, w_array_n*2, 2, ',', &data_q);
  
  double* j_array;
  int j_array_n = read_table_row(double)("SDF.txt", 0, 1, 0, 2, ',', &j_array);
  
  double** data_asprat;
  double** data_qstar;
  int data_asprat_n = read_table(double)("SDF.txt", 1, 0, w_array_n*2, 2, ',', &data_asprat);
  int data_qstar_n = read_table(double)("SDF.txt", 1, 1, w_array_n*2, 2, ',', &data_qstar);
  
  double q = calc_q(w_array, w_array_n, data_sd, data_sd_n, data_q, data_q_n, params);
  
  double* q_vec;
  int q_vec_n = calc_q_vec(j_array, j_array_n, data_asprat, data_asprat_n, data_qstar, params, &q_vec);
  
  double* j_vec;
  int j_vec_n = calc_j_vec(j_array, j_array_n, data_asprat, data_asprat_n, data_qstar, params, &j_vec);

  double q_hat = calc_q_hat(q, q_vec, q_vec_n, params);
  double j_tol = calc_j_tol(params);
  double j = calc_j(q_vec, q_vec_n, j_vec, q_hat);
  double q_hat_tol = calc_q_hat_tol(q_vec, j_vec, j_vec_n, j_tol);
  double pb = calc_pb(j, params);
  double nfl = calc_nfl(q_hat_tol, params); 
  double lr = calc_lr(nfl, params);
  int is_safe1 = calc_is_safe1(pb, params);
  int is_safe2 = calc_is_safe2(lr, q);
  
  display_output("outputfile.txt", q, j, q_hat_tol, pb, lr, nfl, is_safe1, is_safe2, params);
  
}