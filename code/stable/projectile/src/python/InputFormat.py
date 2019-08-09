## \file InputFormat.py
# \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
# \brief Provides the function for reading inputs
from __future__ import print_function
import sys
import math

import InputParameters

## \brief Reads input from a file with the given file name
# \param filename name of the input file
# \param inParams structure holding the input values
def get_input(filename, inParams):
    infile = open(filename, "r")
    infile.readline()
    inParams.v_launch = float(infile.readline())
    infile.readline()
    inParams.theta = float(infile.readline())
    infile.readline()
    inParams.p_target = float(infile.readline())
    infile.close()


