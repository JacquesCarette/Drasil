#ifndef ARRAYLIB_H
#define ARRAYLIB_H

#define max(T) max_##T
#define min(T) min_##T
#define genAbs(T) genAbs_##T
#define arrayMin(T) arrayMin_##T
#define arrayMax(T) arrayMax_##T
#define arrayArgMin(T) arrayArgMin_##T
#define arrayArgMax(T) arrayArgMax_##T
#define arrayAdd(T) arrayAdd_##T
#define arraySubt(T) arraySubt_##T
#define arrayMult(T) arrayMult_##T
#define arrayDivide(T) arrayDivide_##T
#define arrayAbs(T) arrayAbs_##T
#define arrayContains(T) arrayContains_##T
#define matrixCol(T) matrixCol_##T

#define declare_generics(T) \
T max_##T(T x, T y); \
T min_##T(T x, T y); \
T genAbs_##T(T x); \
T arrayMin_##T(T *arr, int n); \
T arrayMax_##T(T *arr, int n); \
int arrayArgMin_##T(T *arr, int n); \
int arrayArgMax_##T(T *arr, int n); \
T *arrayAdd_##T(T *arr, int n, T x); \
T *arraySubt_##T(T *arr, int n, T x); \
T *arrayMult_##T(T *arr, int n, T x); \
T *arrayDivide_##T(T *arr, int n, T x); \
T *arrayAbs_##T(T *arr, int n); \
int arrayContains_##T(T *arr, int n, T x); \
T *matrixCol_##T(T **arr, int n, int j);

declare_generics(int)
declare_generics(double)
declare_generics(float)
declare_generics(char)

#endif /* ARRAYLIB_H */