from __future__ import print_function
import sys
import math


def lin_interp(x1, y1, x2, y2, x):
    y = (((y2 - y1) / (x2 - x1)) * (x - x1)) + y1
    return y

def indInSeq(arr, v):
    for i in range(len(arr) - 1):
        if ((arr[i] <= v) and (v <= arr[i + 1])) :
            return i
    raise Exception("Index not found")

def matrixCol(mat, c):
    col = []
    for i in range(len(mat)):
        col.append(mat[i][c])
    return col

def interpY(x_array, y_array, z_array, x, z):
    i = indInSeq(z_array, z)
    x_z1 = matrixCol(x_array, i)
    y_z1 = matrixCol(y_array, i)
    x_z2 = matrixCol(x_array, i + 1)
    y_z2 = matrixCol(y_array, i + 1)
    try :
        j = indInSeq(x_z1, x)
        k = indInSeq(x_z2, x)
    except Exception as exc :
        raise Exception("Interpolation of y failed.")
    y1 = lin_interp(x_z1[j], y_z1[j], x_z1[j + 1], y_z1[j + 1], x)
    y2 = lin_interp(x_z2[k], y_z2[k], x_z2[k + 1], y_z2[k + 1], x)
    return lin_interp(z_array[i], y1, z_array[i + 1], y2, z)

def interpZ(x_array, y_array, z_array, x, y):
    for i in range(len(z_array) - 1):
        x_z1 = matrixCol(x_array, i)
        y_z1 = matrixCol(y_array, i)
        x_z2 = matrixCol(x_array, i + 1)
        y_z2 = matrixCol(y_array, i + 1)
        try :
            j = indInSeq(x_z1, x)
            k = indInSeq(x_z2, x)
        except Exception as exc :
            continue
        y_lower = lin_interp(x_z1[j], y_z1[j], x_z1[j + 1], y_z1[j + 1], x)
        y_upper = lin_interp(x_z2[k], y_z2[k], x_z2[k + 1], y_z2[k + 1], x)
        if ((y_lower <= y) and (y <= y_upper)) :
            return lin_interp(y_lower, z_array[i], y_upper, z_array[i + 1], y)
    raise Exception("Interpolation of z failed.")


