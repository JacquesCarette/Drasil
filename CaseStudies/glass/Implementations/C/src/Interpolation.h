#ifndef INTERPOLATION_H
#define INTERPOLATION_H

double lin_interp(double y1, double y2, double x1, double x2, double input_param);
int proper_index(int index1, int index2, double** data, int data_n, double value);
int find_idx(double* data1, int data1_n, double value1);
int find_jdx(double** data2, int data2_n, double value2, int idx);
int find_kdx(double** data2, int data2_n, double value2, int idx);
int find_num_interp1(double* data1, int data1_n, double value1, int idx); 
int find_num_interp2(double* data1, int data1_n, double** data2, int data2_n, double value1, double value2, int idx);
double interp(int idx, int jdx, int kdx, int num_interp1, int num_interp2, double* data1, double** data2, double** data3, double value1, double value2);

#endif /* INTERPOLATION_H */