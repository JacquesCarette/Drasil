"""
This module implements a portion of the Input Format Module.  In this
case the input is the tabular data necessary for the different interpolations.
"""

import numpy as np


def read_num_col(filename):
    with open(filename, 'rb') as f:
        num_col = [f.readline()]

    num_col = np.genfromtxt(num_col, delimiter=',', dtype=str)
    num_col = num_col[1::2].astype(float)
    
    return num_col
    
def read_array1(filename, length):
    array1 = np.loadtxt(filename, delimiter=',', usecols=range(0, 2*length, 2), skiprows=1)
    return array1
    
def read_array2(filename, length):
    array2 = np.loadtxt(filename, delimiter=',', usecols=range(1, 2*length, 2), skiprows=1)
    return array2
