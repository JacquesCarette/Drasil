from __future__ import print_function
import sys
import math
import InputParameters


def func_get_input(filename, inParams):
    infile = open(filename, "r")
    infile.readline()
    inParams.v = float(infile.readline())
    infile.readline()
    inParams.theta = float(infile.readline())
    infile.readline()
    inParams.p_target = float(infile.readline())
    infile.close()


