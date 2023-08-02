## \file DerivedValues.py
# \author Nikitha Krithnan and W. Spencer Smith
# \brief Provides the function for calculating derived values
import math

## \brief Calculates values that can be immediately derived from the inputs
# \param inParams structure holding the input values
def derived_values(inParams):
    outfile = open("log.txt", "a")
    print("function derived_values called with inputs: {", file=outfile)
    print("  inParams = ", end="", file=outfile)
    print("Instance of InputParameters object", file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    inParams.LDF = (3.0 / 60.0) ** (7.0 / 16.0)
    outfile = open("log.txt", "a")
    print("var 'inParams.LDF' assigned ", end="", file=outfile)
    print(inParams.LDF, end="", file=outfile)
    print(" in module DerivedValues", file=outfile)
    outfile.close()
    
    inParams.SD = math.sqrt(inParams.SD_x ** 2.0 + inParams.SD_y ** 2.0 + inParams.SD_z ** 2.0)
    outfile = open("log.txt", "a")
    print("var 'inParams.SD' assigned ", end="", file=outfile)
    print(inParams.SD, end="", file=outfile)
    print(" in module DerivedValues", file=outfile)
    outfile.close()
    
    inParams.w_TNT = inParams.w * inParams.TNT
    outfile = open("log.txt", "a")
    print("var 'inParams.w_TNT' assigned ", end="", file=outfile)
    print(inParams.w_TNT, end="", file=outfile)
    print(" in module DerivedValues", file=outfile)
    outfile.close()
