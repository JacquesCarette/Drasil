from __future__ import print_function
import sys
import math

def write_output(is_safePb, is_safeLR, P_b):
    outputfile = open("output.txt", "w")
    print("is_safePb = ", end='', file=outputfile)
    print(is_safePb, file=outputfile)
    print("is_safeLR = ", end='', file=outputfile)
    print(is_safeLR, file=outputfile)
    print("P_b = ", end='', file=outputfile)
    print(P_b, file=outputfile)
    outputfile.close()


