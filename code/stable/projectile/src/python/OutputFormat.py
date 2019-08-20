## \file OutputFormat.py
# \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
# \brief Provides the function for writing outputs
from __future__ import print_function
import sys
import math

## \brief Writes the output values to output.txt
# \param s output message as a string
# \param d_offset distance between the target position and the landing position: the offset between the target position and the landing position (m)
def write_output(s, d_offset):
    outputfile = open("output.txt", "w")
    print("s = ", end='', file=outputfile)
    print(s, file=outputfile)
    print("d_offset = ", end='', file=outputfile)
    print(d_offset, file=outputfile)
    outputfile.close()


