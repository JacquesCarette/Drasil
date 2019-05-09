from __future__ import print_function
import sys
import math


def write_output(is_safePb, is_safeLR, P_b):
    outfile = open("output.txt", "w")
    print("is_safePb = ", end='', file=outfile)
    print(is_safePb, file=outfile)
    print("is_safeLR = ", end='', file=outfile)
    print(is_safeLR, file=outfile)
    print("P_b = ", end='', file=outfile)
    print(P_b, file=outfile)
    outfile.close()


