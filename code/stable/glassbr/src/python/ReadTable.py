from __future__ import print_function
import sys
import math


def func_read_table(filename, z_vector, x_matrix, y_matrix):
    lines = []
    linetokens = []
    infile = open(filename, "r")
    line = infile.readline()
    linetokens = line.split(",")
    for j in range(0, int(len(linetokens) / 2)):
        while (len(z_vector) <= j) :
            z_vector.append(0.0)
        z_vector[j] = float(linetokens[(j * 2) + 1])
    lines = infile.readlines()
    for i in range(0, len(lines)):
        linetokens = lines[i].split(",")
        for j in range(0, int(len(linetokens) / 2)):
            while (len(x_matrix) <= i) :
                x_matrix.append([])
            while (len(x_matrix[i]) <= j) :
                x_matrix[i].append(0.0)
            x_matrix[i][j] = float(linetokens[(j * 2) + 0])
            while (len(y_matrix) <= i) :
                y_matrix.append([])
            while (len(y_matrix[i]) <= j) :
                y_matrix[i].append(0.0)
            y_matrix[i][j] = float(linetokens[(j * 2) + 1])
    infile.close()


