## \file OutputFormat.py
# \brief Provides the function for writing outputs
from __future__ import print_function
import sys
import math

## \brief Writes the output values to output.txt
# \param T_W temperature of the water
# \param E_W change in heat energy in the water
def write_output(T_W, E_W):
    outputfile = open("output.txt", "w")
    print("T_W = ", end='', file=outputfile)
    print(T_W, file=outputfile)
    print("E_W = ", end='', file=outputfile)
    print(E_W, file=outputfile)
    outputfile.close()


