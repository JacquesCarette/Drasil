from __future__ import print_function
import sys
import math


def read_z_array(filename):
    infile = open(filename, "r")
    line = infile.readline()
    infile.close()
    z_array_str = []
    z_array_str = line.split(",")
    z_array_str = z_array_str[1::2]
    z_array = []
    for i in range(len(z_array_str)):
        z_array.append(float(z_array_str[i]))
    return z_array

def read_x_array(filename):
    infile = open(filename, "r")
    lines = []
    lines = infile.readlines()
    infile.close()
    lines = lines[1::]
    x_array_str = []
    for i in range(len(lines)):
        temp_str = []
        temp_str = lines[i].split(",")
        temp_str = temp_str[0::2]
        x_array_str.append(temp_str)
    x_array = []
    for i in range(len(x_array_str)):
        nextLine = []
        for j in range(len(x_array_str[i])):
            nextLine.append(float(x_array_str[i][j]))
        x_array.append(nextLine)
    return x_array

def read_y_array(filename):
    infile = open(filename, "r")
    lines = []
    lines = infile.readlines()
    infile.close()
    lines = lines[1::]
    y_array_str = []
    for i in range(len(lines)):
        temp_str = []
        temp_str = lines[i].split(",")
        temp_str = temp_str[1::2]
        y_array_str.append(temp_str)
    y_array = []
    for i in range(len(y_array_str)):
        nextLine = []
        for j in range(len(y_array_str[i])):
            nextLine.append(float(y_array_str[i][j]))
        y_array.append(nextLine)
    return y_array


