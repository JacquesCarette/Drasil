from __future__ import print_function
import sys
import math


def write_output(hit, short, offset):
    outfile = open("output.txt", "w")
    if hit:
        print("The target was hit.", file=outfile)
    elif short:
        print("The shot came up short by ", end='', file=outfile)
        print(offset, end='', file=outfile)
        print(" metres.", file=outfile)
    else:
        print("The shot went long by ", end='', file=outfile)
        print(offset, end='', file=outfile)
        print(" metres.", file=outfile)
    outfile.close()


