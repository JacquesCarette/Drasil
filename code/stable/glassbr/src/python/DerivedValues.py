## \file DerivedValues.py
# \author Nikitha Krithnan and W. Spencer Smith
# \brief Provides the function for calculating derived values
from __future__ import print_function
import sys
import math

import InputParameters

## \brief Calculates values that can be immediately derived from the inputs
# \param inParams structure holding the input values
def derived_values(inParams):
    outfile = open("log.txt", "a")
    print("function derived_values called with inputs: {", file=outfile)
    print("  inParams = ", end='', file=outfile)
    print("Instance of InputParameters object", file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    inParams.h = 1.0 / 1000.0 * (2.16 if inParams.t == 2.5 else 2.59 if inParams.t == 2.7 else 2.92 if inParams.t == 3.0 else 3.78 if inParams.t == 4.0 else 4.57 if inParams.t == 5.0 else 5.56 if inParams.t == 6.0 else 7.42 if inParams.t == 8.0 else 9.02 if inParams.t == 10.0 else 11.91 if inParams.t == 12.0 else 15.09 if inParams.t == 16.0 else 18.26 if inParams.t == 19.0 else 21.44)
    outfile = open("log.txt", "a")
    print("var 'inParams.h' assigned to ", end='', file=outfile)
    print(inParams.h, end='', file=outfile)
    print(" in module DerivedValues", file=outfile)
    outfile.close()
    
    inParams.LDF = (3.0 / 60) ** (7.0 / 16)
    outfile = open("log.txt", "a")
    print("var 'inParams.LDF' assigned to ", end='', file=outfile)
    print(inParams.LDF, end='', file=outfile)
    print(" in module DerivedValues", file=outfile)
    outfile.close()
    
    if (inParams.g == "AN") :
        inParams.GTF = 1
        outfile = open("log.txt", "a")
        print("var 'inParams.GTF' assigned to ", end='', file=outfile)
        print(inParams.GTF, end='', file=outfile)
        print(" in module DerivedValues", file=outfile)
        outfile.close()
    elif (inParams.g == "FT") :
        inParams.GTF = 4
        outfile = open("log.txt", "a")
        print("var 'inParams.GTF' assigned to ", end='', file=outfile)
        print(inParams.GTF, end='', file=outfile)
        print(" in module DerivedValues", file=outfile)
        outfile.close()
    elif (inParams.g == "HS") :
        inParams.GTF = 2
        outfile = open("log.txt", "a")
        print("var 'inParams.GTF' assigned to ", end='', file=outfile)
        print(inParams.GTF, end='', file=outfile)
        print(" in module DerivedValues", file=outfile)
        outfile.close()
    else :
        raise Exception("Undefined case encountered in function GTF")
    
    inParams.SD = math.sqrt(inParams.SD_x ** 2 + inParams.SD_y ** 2 + inParams.SD_z ** 2)
    outfile = open("log.txt", "a")
    print("var 'inParams.SD' assigned to ", end='', file=outfile)
    print(inParams.SD, end='', file=outfile)
    print(" in module DerivedValues", file=outfile)
    outfile.close()
    
    inParams.AR = inParams.a / inParams.b
    outfile = open("log.txt", "a")
    print("var 'inParams.AR' assigned to ", end='', file=outfile)
    print(inParams.AR, end='', file=outfile)
    print(" in module DerivedValues", file=outfile)
    outfile.close()
    
    inParams.w_TNT = inParams.w * inParams.TNT
    outfile = open("log.txt", "a")
    print("var 'inParams.w_TNT' assigned to ", end='', file=outfile)
    print(inParams.w_TNT, end='', file=outfile)
    print(" in module DerivedValues", file=outfile)
    outfile.close()


