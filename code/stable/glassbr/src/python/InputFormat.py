from __future__ import print_function
import sys
import math

import InputParameters

def get_input(filename, inParams):
    outfile = open("log.txt", "a")
    print("function get_input called with inputs: {", file=outfile)
    print("  filename = ", end='', file=outfile)
    print(filename, end='', file=outfile)
    print(", ", file=outfile)
    print("  inParams = ", end='', file=outfile)
    print("Instance of InputParameters object", file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    lines = []
    linetokens = []
    infile = open(filename, "r")
    infile.readline()
    inParams.a = float(infile.readline())
    infile.readline()
    inParams.b = float(infile.readline())
    infile.readline()
    inParams.w = float(infile.readline())
    infile.readline()
    inParams.P_btol = float(infile.readline())
    infile.readline()
    inParams.TNT = float(infile.readline())
    infile.readline()
    inParams.g = infile.readline().rstrip()
    infile.readline()
    inParams.t = float(infile.readline())
    infile.readline()
    inParams.SD_x = float(infile.readline())
    infile.readline()
    inParams.SD_y = float(infile.readline())
    infile.readline()
    inParams.SD_z = float(infile.readline())
    infile.close()


