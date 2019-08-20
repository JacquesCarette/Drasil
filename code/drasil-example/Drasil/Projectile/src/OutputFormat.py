from __future__ import print_function
import sys
import math


def write_output(message, offset):
    outfile = open("output.txt", "w")
    print(message, file=outfile)
    print("Offset: ", end='', file=outfile)
    print(offset, file=outfile)
    outfile.close()


