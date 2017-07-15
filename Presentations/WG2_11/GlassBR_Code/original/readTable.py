"""
This module implements a portion of the Input Format Module.  In this
case the input is the tabular data necessary for the different interpolations.
"""

import numpy as np

def read_z_array(filename):
    with open(filename, "r") as f:
        line = f.readline()
    z_array = line.split(",")[1::2]
    z_array = [float(i) for i in z_array]  
    return z_array
    
def read_x_array(filename, length):
    with open(filename, "r") as f:
        lines = f.readlines()
    lines = lines[1:]
    x_array = [line.split(",")[0::2] for line in lines]
    for i in range(len(x_array)):
        x_array[i] = [float(j) for j in x_array[i]]
    return x_array
    
def read_y_array(filename, length):
    with open(filename, "r") as f:
        lines = f.readlines()
    lines = lines[1:]
    y_array = [line.split(",")[1::2] for line in lines]
    for i in range(len(y_array)):
        y_array[i] = [float(j) for j in y_array[i]]
    return y_array

#---#

#older function; needs to be removed#

def read_table(filename):
    with open(filename, 'rb') as f:
        num_col = [f.readline()]

    num_col = np.genfromtxt(num_col, delimiter=',', dtype=str)
    num_col = num_col[1::2].astype(float)

    array1 = np.loadtxt(filename, delimiter=',', usecols=range(0, 2*len(num_col), 2), skiprows=1)
    array2 = np.loadtxt(filename, delimiter=',', usecols=range(1, 2*len(num_col), 2), skiprows=1)

    return num_col, array1, array2
