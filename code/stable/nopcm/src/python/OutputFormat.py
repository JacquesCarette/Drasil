from __future__ import print_function
import sys
import math


def write_output(inParams):
    outfile = open("output.txt", "w")
    print("T_W = ", end='', file=outfile)
    print(inParams.T_W, file=outfile)
    print("E_W = ", end='', file=outfile)
    print(inParams.E_W, file=outfile)
    outfile.close()


