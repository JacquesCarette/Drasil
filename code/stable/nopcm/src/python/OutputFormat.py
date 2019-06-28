from __future__ import print_function
import sys
import math

def write_output(T_W, E_W):
    outputfile = open("output.txt", "w")
    print("T_W = ", end='', file=outputfile)
    print(T_W, file=outputfile)
    print("E_W = ", end='', file=outputfile)
    print(E_W, file=outputfile)
    outputfile.close()


