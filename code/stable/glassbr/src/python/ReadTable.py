## \file ReadTable.py
# \brief Provides a function for reading glass ASTM data
from __future__ import print_function
import sys
import math

## \brief Reads glass ASTM data from a file with the given file name
# \param filename No description given
# \param z_vector No description given
# \param x_matrix No description given
# \param y_matrix No description given
def func_read_table(filename, z_vector, x_matrix, y_matrix):
    outfile = open("log.txt", "a")
    print("function func_read_table called with inputs: {", file=outfile)
    print("  filename = ", end='', file=outfile)
    print(filename, end='', file=outfile)
    print(", ", file=outfile)
    print("  z_vector = ", end='', file=outfile)
    print(z_vector, end='', file=outfile)
    print(", ", file=outfile)
    print("  x_matrix = ", end='', file=outfile)
    print(x_matrix, end='', file=outfile)
    print(", ", file=outfile)
    print("  y_matrix = ", end='', file=outfile)
    print(y_matrix, file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    lines = []
    linetokens = []
    infile = open(filename, "r")
    line = infile.readline().rstrip()
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


