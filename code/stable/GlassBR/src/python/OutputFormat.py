## \file OutputFormat.py
# \author Nikitha Krithnan and W. Spencer Smith
# \brief Provides the function for writing outputs
## \brief Writes the output values to output.txt
# \param is_safePb probability of glass breakage safety requirement
# \param is_safeLR 3 second load equivalent resistance safety requirement
# \param P_b probability of breakage: the fraction of glass lites or plies that would break at the first occurrence of a specified load and duration, typically expressed in lites per 1000 (Ref: astm2016)
# \param J stress distribution factor (Function)
def write_output(is_safePb, is_safeLR, P_b, J):
    outfile = open("log.txt", "a")
    print("function write_output called with inputs: {", file=outfile)
    print("  is_safePb = ", end="", file=outfile)
    print(is_safePb, end="", file=outfile)
    print(", ", file=outfile)
    print("  is_safeLR = ", end="", file=outfile)
    print(is_safeLR, end="", file=outfile)
    print(", ", file=outfile)
    print("  P_b = ", end="", file=outfile)
    print(P_b, end="", file=outfile)
    print(", ", file=outfile)
    print("  J = ", end="", file=outfile)
    print(J, file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    outputfile = open("output.txt", "w")
    print("is_safePb = ", end="", file=outputfile)
    print(is_safePb, file=outputfile)
    print("is_safeLR = ", end="", file=outputfile)
    print(is_safeLR, file=outputfile)
    print("P_b = ", end="", file=outputfile)
    print(P_b, file=outputfile)
    print("J = ", end="", file=outputfile)
    print(J, file=outputfile)
    outputfile.close()
