from __future__ import print_function
import sys
import math
import ReadTable


def func_lin_interp(x_1, y_1, x_2, y_2, x):
    return (((y_2 - y_1) / (x_2 - x_1)) * (x - x_1)) + y_1

def func_find(arr, v):
    for i in range(0, len(arr) - 1):
        if ((arr[i] <= v) and (v <= arr[i + 1])) :
            return i
    raise Exception("Bound error")

def func_extractColumn(mat, j):
    col = []
    for i in range(0, len(mat)):
        col.append(mat[i][j])
    return col

def func_interpY(filename, x, z):
    x_matrix = []
    y_matrix = []
    z_vector = []
    ReadTable.func_read_table(filename, z_vector, x_matrix, y_matrix)
    i = func_find(z_vector, z)
    x_z_1 = func_extractColumn(x_matrix, i)
    y_z_1 = func_extractColumn(y_matrix, i)
    x_z_2 = func_extractColumn(x_matrix, i + 1)
    y_z_2 = func_extractColumn(y_matrix, i + 1)
    try :
        j = func_find(x_z_1, x)
        k_2 = func_find(x_z_2, x)
    except Exception as exc :
        raise Exception("Interpolation of y failed")
    y_1 = func_lin_interp(x_z_1[j], y_z_1[j], x_z_1[j + 1], y_z_1[j + 1], x)
    y_2 = func_lin_interp(x_z_2[k_2], y_z_2[k_2], x_z_2[k_2 + 1], y_z_2[k_2 + 1], x)
    return func_lin_interp(z_vector[i], y_1, z_vector[i + 1], y_2, z)

def func_interpZ(filename, x, y):
    x_matrix = []
    y_matrix = []
    z_vector = []
    ReadTable.func_read_table(filename, z_vector, x_matrix, y_matrix)
    for i in range(0, len(z_vector) - 1):
        x_z_1 = func_extractColumn(x_matrix, i)
        y_z_1 = func_extractColumn(y_matrix, i)
        x_z_2 = func_extractColumn(x_matrix, i + 1)
        y_z_2 = func_extractColumn(y_matrix, i + 1)
        try :
            j = func_find(x_z_1, x)
            k_2 = func_find(x_z_2, x)
        except Exception as exc :
            continue
        y_1 = func_lin_interp(x_z_1[j], y_z_1[j], x_z_1[j + 1], y_z_1[j + 1], x)
        y_2 = func_lin_interp(x_z_2[k_2], y_z_2[k_2], x_z_2[k_2 + 1], y_z_2[k_2 + 1], x)
        if ((y_1 <= y) and (y <= y_2)) :
            return func_lin_interp(y_1, z_vector[i], y_2, z_vector[i + 1], y)
    raise Exception("Interpolation of z failed")


