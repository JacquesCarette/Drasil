#include "ArrayLib.h"

#include <stdlib.h>

#define define_functions(T) \
T max_##T(T x, T y){ \
  return x > y ? x : y; \
} \
\
T min_##T(T x, T y){ \
  return x < y ? x : y; \
} \
\
T genAbs_##T(T x){ \
  return x < 0 ? -x : x; \
} \
\
T arrayMin_##T(T *arr, int n){ \
  T currentMin = arr[0]; \
  for(int i = 1; i < n; i++) \
    currentMin = min_##T(currentMin, arr[i]); \
  return currentMin; \
} \
\
T arrayMax_##T(T *arr, int n){ \
  T currentMax = arr[0]; \
  for(int i = 1; i < n; i++) \
    currentMax = max_##T(currentMax, arr[i]); \
  return currentMax; \
} \
\
int arrayArgMin_##T(T *arr, int n){ \
  T currentMin = arr[0]; \
  T newMin; \
  int index = 0; \
  for(int i = 1; i < n; i++) { \
    newMin = min_##T(currentMin, arr[i]); \
    if (newMin < currentMin) { \
      currentMin = newMin; \
      index = i; \
    } \
  } \
  return index; \
} \
\
int arrayArgMax_##T(T *arr, int n){ \
  T currentMax = arr[0]; \
  T newMax; \
  int index = 0; \
  for(int i = 1; i < n; i++) { \
    newMax = max_##T(currentMax, arr[i]); \
    if (newMax > currentMax) { \
      currentMax = newMax; \
      index = i; \
    } \
  return index; \
  } \
} \
\
T *arrayAdd_##T(T *arr, int n, T x){ \
  T *newArr = (T*)malloc(sizeof(T) * n); \
  for(int i = 0; i < n; i++) \
    newArr[i] = arr[i] + x; \
  return newArr; \
} \
\
T *arraySubt_##T(T *arr, int n, T x){ \
  T *newArr = (T*)malloc(sizeof(T) * n); \
  for(int i = 0; i < n; i++) \
    newArr[i] = arr[i] - x; \
  return newArr; \
} \
\
T *arrayMult_##T(T *arr, int n, T x){ \
  T *newArr = (T*)malloc(sizeof(T) * n); \
  for(int i = 0; i < n; i++) \
    newArr[i] = arr[i] * x; \
  return newArr; \
} \
\
T *arrayDivide_##T(T *arr, int n, T x){ \
  T *newArr = (T*)malloc(sizeof(T) * n); \
  for(int i = 0; i < n; i++) \
    newArr[i] = arr[i] / x; \
  return newArr; \
} \
\
T *arrayAbs_##T(T *arr, int n){ \
  T *newArr = (T*)malloc(sizeof(T) * n); \
  for(int i = 0; i < n; i++) \
    newArr[i] = genAbs_##T(arr[i]); \
  return newArr; \
} \
\
T *matrixCol_##T(T **arr, int n, int j){ \
  T *newArr = (T*)malloc(sizeof(T) * n); \
  for(int i = 0; i < n; i++) \
    newArr[i] = arr[i][j]; \
  return newArr; \
} \
\
int arrayContains_##T(T *arr, int n, T x){ \
  for(int i = 0; i < n; i++) \
    if(arr[i] == x) return 1; \
  return 0; \
}


define_functions(int)
define_functions(double)
define_functions(float)
define_functions(char)