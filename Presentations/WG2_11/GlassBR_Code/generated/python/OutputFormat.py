from __future__ import print_function
import sys
import math


def write_output(filename, is_safe1, is_safe2, P_b):
    outfile = open(filename, "w")
    print("is_safe1 = ", end='', file=outfile);
    print(is_safe1, file=outfile);
    print("is_safe2 = ", end='', file=outfile);
    print(is_safe2, file=outfile);
    print("P_b = ", end='', file=outfile);
    print(P_b, file=outfile);
    outfile.close()


