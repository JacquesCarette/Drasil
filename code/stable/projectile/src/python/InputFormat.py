## \file InputFormat.py
# \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
# \brief Provides the function for reading inputs
from __future__ import print_function
import sys
import math

import InputParameters

## \brief Reads input from a file with the given file name
# \param inParams structure holding the input values
# \param filename name of the input file
def get_input(inParams, filename):
    infile = open(filename, "r")
    infile.readline()
    inParams.v_launch = float(infile.readline())
    infile.readline()
    inParams.theta = float(infile.readline())
    infile.readline()
    inParams.p_target = float(infile.readline())
    infile.close()
