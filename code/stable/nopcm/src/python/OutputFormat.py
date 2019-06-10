from __future__ import print_function
import sys
import math

def write_output(inParams):
    outputfile = open("output.txt", "w")
    print("T_W = ", end='', file=outputfile)
    print(inParams.T_W, file=outputfile)
    print("E_W = ", end='', file=outputfile)
    print(inParams.E_W, file=outputfile)
    outputfile.close()


