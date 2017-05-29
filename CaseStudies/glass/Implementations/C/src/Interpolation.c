#include "Interpolation.h"

#include "ArrayLib.h"

double lin_interp(double y1, double y2, double x1, double x2, double input_param) {
    float y0 = y1 + (y2 - y1)/(x2 - x1)*(input_param - x1);
    return y0;
}

int proper_index(int index1, int index2, double** data, int data_n, double value) {
    double index2Max = arrayMax(double)(matrixCol(double)(data, data_n, index2), data_n);
    if (index1 == 0) {
        if (data[index1][index2] == data[index1+1][index2]) {
            index1 += 1;
        }
    }
    else if (index1 == index2Max) {
        index1 -= 1;
    }
    else {
        if (data[index1][index2] > value)
            index1 -= 1;
        else if (data[index1][index2] < value) {
            if (index1+1 < index2Max && data[index1][index2] == data[index1+1][index2])
                index1 += 1;
            else if (index1+1 == index2Max && data[index1][index2] == data[index1+1][index2])
                index1 -= 1;
        }
    }
    return index1;
}

int find_idx(double* data1, int data1_n, double value1) {
    int idx = arrayArgMin(double)(arrayAbs(double)(arraySubt(double)(data1, data1_n, value1), data1_n), data1_n);
    if (data1[idx] > value1)
        idx -= 1;
    return idx;
}

int find_jdx(double** data2, int data2_n, double value2, int idx) { 
    int jdx = arrayArgMin(double)(arrayAbs(double)(arraySubt(double)(matrixCol(double)(data2, data2_n, idx), data2_n, value2), data2_n), data2_n);
    jdx = proper_index(jdx,idx,data2,data2_n,value2);
    return jdx;
}
    
int find_kdx(double** data2, int data2_n, double value2, int idx) { 
    int kdx = arrayArgMin(double)(arrayAbs(double)(arraySubt(double)(matrixCol(double)(data2, data2_n, idx + 1), data2_n, value2), data2_n), data2_n);
    kdx = proper_index(kdx,idx+1,data2,data2_n,value2);
    return kdx;
}

int find_num_interp1(double* data1, int data1_n, double value1, int idx) {
    int num_interp1;
    if (arrayContains(double)(data1, data1_n, value1))
        num_interp1 = 0;
    else
        num_interp1 = 1;
    return num_interp1;
}
    
int find_num_interp2(double* data1, int data1_n, double** data2, int data2_n, double value1, double value2, int idx) {
    int num_interp2;
    int p1 = arrayContains(double)(matrixCol(double)(data2, data2_n, idx), data2_n, value2);
    int p2 = arrayContains(double)(matrixCol(double)(data2, data2_n, idx + 1), data2_n, value2);
    
    if (arrayContains(double)(data1, data1_n, value1)) {
        if (p1)
            num_interp2 = 0;
        else
            num_interp2 = 1; 
    }
    else {
        if (p1 && p2)
            num_interp2 = 0;
        else if (p1)
            num_interp2 = 1;
        else if (p2)
            num_interp2 = 2;
        else
            num_interp2 = 3;
    }
    return num_interp2;
}

double interp(int idx, int jdx, int kdx, int num_interp1, int num_interp2, double* data1, double** data2, double** data3, double value1, double value2) {
    double interp_value;
    if (num_interp1 == 0 && num_interp2 == 0)
        interp_value = data3[jdx][idx];
    else if (num_interp1 == 0 && num_interp2 == 1)
        interp_value = lin_interp(data3[jdx][idx], data3[jdx+1][idx], data2[jdx][idx], data2[jdx+1][idx], value2);
    else if (num_interp1 == 1 && num_interp2 == 0)
        interp_value = lin_interp(data3[jdx][idx], data3[kdx][idx+1], data1[idx], data1[idx+1], value1);
    else if (num_interp1 == 1 && num_interp2 == 1) {
        double y0_1 = data3[jdx][idx];
        double y0_2 = lin_interp(data3[kdx][idx+1], data3[kdx+1][idx+1], data2[kdx][idx+1], data2[kdx+1][idx+1], value2);
        interp_value = lin_interp(y0_1, y0_2, data1[idx], data1[idx+1], value1);
    }
    else if (num_interp1 == 1 && num_interp2 ==2) {
        double y0_2 = data3[kdx][idx+1];
        double y0_1 = lin_interp(data3[jdx][idx], data3[jdx+1][idx], data2[jdx][idx], data2[jdx+1][idx], value2);
        double x1_1 = data1[idx];
        double x1_2 = data1[idx+1];
        interp_value = lin_interp(y0_1, y0_2, x1_1, x1_2, value1);
    }
    else if (num_interp1 == 1 && num_interp2 == 3) {
        double y0_1 = lin_interp(data3[jdx][idx], data3[jdx+1][idx], data2[jdx][idx], data2[jdx+1][idx], value2);
        double y0_2 = lin_interp(data3[kdx][idx+1], data3[kdx+1][idx+1], data2[kdx][idx+1], data2[kdx+1][idx+1], value2);
        double x0_1 = data1[idx];
        double x0_2 = data1[idx+1];
        interp_value = lin_interp(y0_1, y0_2, x0_1, x0_2, value1);
    }
    return interp_value;
}