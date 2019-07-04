from __future__ import print_function
import sys
import math

import InputParameters

def get_input(filename, inParams):
    lines = []
    linetokens = []
    infile = open(filename, "r")
    infile.readline()
    inParams.v_launch = float(infile.readline())
    infile.readline()
    inParams.angle = float(infile.readline())
    infile.readline()
    inParams.p_target = float(infile.readline())
    infile.close()


