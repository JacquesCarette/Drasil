"""
This module implements a portion of the Input Format Module.  In this
case the input is the tabular data necessary for the different interpolations.
"""

import numpy as np


def read_table(filename):
    with open(filename, 'rb') as f:
        num_col = [f.readline()]

    num_col = np.genfromtxt(num_col, delimiter=',', dtype=str)
    num_col = num_col[1::2].astype(float)

    array1 = np.loadtxt(filename, delimiter=',', usecols=range(0, 2*len(num_col), 2), skiprows=1)
    array2 = np.loadtxt(filename, delimiter=',', usecols=range(1, 2*len(num_col), 2), skiprows=1)

    return num_col, array1, array2
