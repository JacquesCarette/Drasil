## \file InputFormat.py
from __future__ import print_function
import sys
import math

import InputParameters

## \brief Reads input from a file with the given file name
# \param filename No description given
# \param inParams No description given
def get_input(filename, inParams):
    infile = open(filename, "r")
    infile.readline()
    inParams.v_launch = float(infile.readline())
    infile.readline()
    inParams.angle = float(infile.readline())
    infile.readline()
    inParams.p_target = float(infile.readline())
    infile.close()


