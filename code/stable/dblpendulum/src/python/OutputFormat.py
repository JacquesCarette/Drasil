## \file OutputFormat.py
# \author Dong Chen
# \brief Provides the function for writing outputs
## \brief Writes the output values to output.txt
# \param theta dependent variables (rad)
def write_output(theta):
    outputfile = open("output.txt", "w")
    print("theta = ", end="", file=outputfile)
    print(theta, file=outputfile)
    outputfile.close()
