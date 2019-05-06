from __future__ import print_function
import sys
import math
import InputParameters


def func_get_input(filename, inParams):
    lines = []
    linetokens = []
    infile = open(filename, "r")
    infile.readline()
    inParams.a = float(infile.readline())
    inParams.b = float(infile.readline())
    inParams.t = float(infile.readline())
    infile.readline()
    inParams.g = infile.readline().rstrip()
    infile.readline()
    inParams.w = float(infile.readline())
    infile.readline()
    inParams.TNT = float(infile.readline())
    infile.readline()
    inParams.SD_x = float(infile.readline())
    inParams.SD_y = float(infile.readline())
    inParams.SD_z = float(infile.readline())
    infile.readline()
    inParams.P_btol = float(infile.readline())
    infile.close()


