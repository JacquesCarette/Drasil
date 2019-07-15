from __future__ import print_function
import sys
import math

def write_output(s, d_offset):
    outputfile = open("output.txt", "w")
    print("s = ", end='', file=outputfile)
    print(s, file=outputfile)
    print("d_offset = ", end='', file=outputfile)
    print(d_offset, file=outputfile)
    outputfile.close()


