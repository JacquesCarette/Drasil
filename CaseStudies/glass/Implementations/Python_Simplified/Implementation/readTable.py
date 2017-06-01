"""
This module implements a portion of the Input Format Module.  In this
case the input is the tabular data necessary for the different interpolations.
"""


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
