## \file OutputFormat.py
# \author Nikitha Krithnan and W. Spencer Smith
# \brief Provides the function for writing outputs
## \brief Writes the output values to output.txt
# \param inParams structure holding the input values
# \param isSafePb Safety Req-Pb
# \param isSafeLR Safety Req-LR
# \param B risk of failure
# \param J stress distribution factor (Function)
# \param NFL non-factored load: three second duration uniform load associated with a probability of breakage less than or equal to 8 lites per 1000 for monolithic AN glass (Pa)
# \param q_hat dimensionless load
# \param q_hat_tol tolerable load
# \param J_tol stress distribution factor (Function) based on Pbtol
def write_output(inParams, isSafePb, isSafeLR, B, J, NFL, q_hat, q_hat_tol, J_tol):
    outfile = open("log.txt", "a")
    print("function write_output called with inputs: {", file=outfile)
    print("  inParams = ", end="", file=outfile)
    print("Instance of InputParameters object", end="", file=outfile)
    print(", ", file=outfile)
    print("  isSafePb = ", end="", file=outfile)
    print(isSafePb, end="", file=outfile)
    print(", ", file=outfile)
    print("  isSafeLR = ", end="", file=outfile)
    print(isSafeLR, end="", file=outfile)
    print(", ", file=outfile)
    print("  B = ", end="", file=outfile)
    print(B, end="", file=outfile)
    print(", ", file=outfile)
    print("  J = ", end="", file=outfile)
    print(J, end="", file=outfile)
    print(", ", file=outfile)
    print("  NFL = ", end="", file=outfile)
    print(NFL, end="", file=outfile)
    print(", ", file=outfile)
    print("  q_hat = ", end="", file=outfile)
    print(q_hat, end="", file=outfile)
    print(", ", file=outfile)
    print("  q_hat_tol = ", end="", file=outfile)
    print(q_hat_tol, end="", file=outfile)
    print(", ", file=outfile)
    print("  J_tol = ", end="", file=outfile)
    print(J_tol, file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    outputfile = open("output.txt", "w")
    print("isSafePb = ", end="", file=outputfile)
    print(isSafePb, file=outputfile)
    print("isSafeLR = ", end="", file=outputfile)
    print(isSafeLR, file=outputfile)
    print("B = ", end="", file=outputfile)
    print(B, file=outputfile)
    print("J = ", end="", file=outputfile)
    print(J, file=outputfile)
    print("NFL = ", end="", file=outputfile)
    print(NFL, file=outputfile)
    print("GTF = ", end="", file=outputfile)
    print(inParams.GTF, file=outputfile)
    print("q_hat = ", end="", file=outputfile)
    print(q_hat, file=outputfile)
    print("q_hat_tol = ", end="", file=outputfile)
    print(q_hat_tol, file=outputfile)
    print("J_tol = ", end="", file=outputfile)
    print(J_tol, file=outputfile)
    print("h = ", end="", file=outputfile)
    print(inParams.h, file=outputfile)
    print("AR = ", end="", file=outputfile)
    print(inParams.AR, file=outputfile)
    outputfile.close()
