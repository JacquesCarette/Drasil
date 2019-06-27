from __future__ import print_function
import sys
import math

def func_read_table(filename, z_vector, x_matrix, y_matrix):
    lines = []
    linetokens = []
    infile = open(filename, "r")
    line = infile.readline()
    linetokens = line.split(",")
    for j in range(0, int((len(linetokens) / 2)), 1):
        z_vector.append(float(linetokens[((j * 2) + 1)]))
    lines = infile.readlines()
    for i in range(0, len(lines), 1):
        linetokens = lines[i].split(",")
        x_matrix_temp = []
        y_matrix_temp = []
        for j in range(0, int((len(linetokens) / 2)), 1):
            x_matrix_temp.append(float(linetokens[((j * 2) + 0)]))
            y_matrix_temp.append(float(linetokens[((j * 2) + 1)]))
        x_matrix.append(x_matrix_temp)
        y_matrix.append(y_matrix_temp)
    infile.close()


