## \file Projectile.py
# \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
# \brief Contains the entire Projectile program
import math
import sys

## \brief Structure for holding the input values
class InputParameters:
    ## \brief Initializes input object by reading inputs and checking physical constraints on the input
    # \param filename name of the input file
    def __init__(self, filename):
        outfile = open("log.txt", "a")
        print("function InputParameters called with inputs: {", file=outfile)
        print("  filename = ", end="", file=outfile)
        print(filename, file=outfile)
        print("  }", file=outfile)
        outfile.close()
        
        self.get_input(filename)
        self.input_constraints()
    
    ## \brief Reads input from a file with the given file name
    # \param filename name of the input file
    def get_input(self, filename):
        outfile = open("log.txt", "a")
        print("function get_input called with inputs: {", file=outfile)
        print("  filename = ", end="", file=outfile)
        print(filename, file=outfile)
        print("  }", file=outfile)
        outfile.close()
        
        infile = open(filename, "r")
        infile.readline()
        self.v_launch = float(infile.readline())
        outfile = open("log.txt", "a")
        print("var 'self.v_launch' assigned ", end="", file=outfile)
        print(self.v_launch, end="", file=outfile)
        print(" in module Projectile", file=outfile)
        outfile.close()
        infile.readline()
        self.theta = float(infile.readline())
        outfile = open("log.txt", "a")
        print("var 'self.theta' assigned ", end="", file=outfile)
        print(self.theta, end="", file=outfile)
        print(" in module Projectile", file=outfile)
        outfile.close()
        infile.readline()
        self.p_target = float(infile.readline())
        outfile = open("log.txt", "a")
        print("var 'self.p_target' assigned ", end="", file=outfile)
        print(self.p_target, end="", file=outfile)
        print(" in module Projectile", file=outfile)
        outfile.close()
        infile.close()
    
    ## \brief Verifies that input values satisfy the physical constraints
    def input_constraints(self):
        outfile = open("log.txt", "a")
        print("function input_constraints called with inputs: {", file=outfile)
        print("  }", file=outfile)
        outfile.close()
        
        if (not(self.v_launch > 0.0)) :
            print("Warning: ", end="")
            print("v_launch has value ", end="")
            print(self.v_launch, end="")
            print(", but is suggested to be ", end="")
            print("above ", end="")
            print(0.0, end="")
            print(".")
        if (not(0.0 < self.theta and self.theta < math.pi / 2.0)) :
            print("Warning: ", end="")
            print("theta has value ", end="")
            print(self.theta, end="")
            print(", but is suggested to be ", end="")
            print("between ", end="")
            print(0.0, end="")
            print(" and ", end="")
            print(math.pi / 2.0, end="")
            print(" ((pi)/(2))", end="")
            print(".")
        if (not(self.p_target > 0.0)) :
            print("Warning: ", end="")
            print("p_target has value ", end="")
            print(self.p_target, end="")
            print(", but is suggested to be ", end="")
            print("above ", end="")
            print(0.0, end="")
            print(".")

## \brief Structure for holding the constant values
class Constants:
    g_vect = 9.8
    epsilon = 2.0e-2

## \brief Calculates flight duration: the time when the projectile lands (s)
# \param inParams structure holding the input values
# \return flight duration: the time when the projectile lands (s)
def func_t_flight(inParams):
    outfile = open("log.txt", "a")
    print("function func_t_flight called with inputs: {", file=outfile)
    print("  inParams = ", end="", file=outfile)
    print("Instance of InputParameters object", file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    return 2.0 * inParams.v_launch * math.sin(inParams.theta) / Constants.g_vect

## \brief Calculates landing position: the distance from the launcher to the final position of the projectile (m)
# \param inParams structure holding the input values
# \return landing position: the distance from the launcher to the final position of the projectile (m)
def func_p_land(inParams):
    outfile = open("log.txt", "a")
    print("function func_p_land called with inputs: {", file=outfile)
    print("  inParams = ", end="", file=outfile)
    print("Instance of InputParameters object", file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    return 2.0 * inParams.v_launch ** 2.0 * math.sin(inParams.theta) * math.cos(inParams.theta) / Constants.g_vect

## \brief Calculates distance between the target position and the landing position: the offset between the target position and the landing position (m)
# \param inParams structure holding the input values
# \param p_land landing position: the distance from the launcher to the final position of the projectile (m)
# \return distance between the target position and the landing position: the offset between the target position and the landing position (m)
def func_d_offset(inParams, p_land):
    outfile = open("log.txt", "a")
    print("function func_d_offset called with inputs: {", file=outfile)
    print("  inParams = ", end="", file=outfile)
    print("Instance of InputParameters object", end="", file=outfile)
    print(", ", file=outfile)
    print("  p_land = ", end="", file=outfile)
    print(p_land, file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    return p_land - inParams.p_target

## \brief Calculates output message as a string
# \param inParams structure holding the input values
# \param d_offset distance between the target position and the landing position: the offset between the target position and the landing position (m)
# \return output message as a string
def func_s(inParams, d_offset):
    outfile = open("log.txt", "a")
    print("function func_s called with inputs: {", file=outfile)
    print("  inParams = ", end="", file=outfile)
    print("Instance of InputParameters object", end="", file=outfile)
    print(", ", file=outfile)
    print("  d_offset = ", end="", file=outfile)
    print(d_offset, file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    if (math.fabs(d_offset / inParams.p_target) < Constants.epsilon) :
        return "The target was hit."
    elif (d_offset < 0.0) :
        return "The projectile fell short."
    else :
        return "The projectile went long."

## \brief Writes the output values to output.txt
# \param s output message as a string
# \param d_offset distance between the target position and the landing position: the offset between the target position and the landing position (m)
# \param t_flight flight duration: the time when the projectile lands (s)
def write_output(s, d_offset, t_flight):
    outfile = open("log.txt", "a")
    print("function write_output called with inputs: {", file=outfile)
    print("  s = ", end="", file=outfile)
    print(s, end="", file=outfile)
    print(", ", file=outfile)
    print("  d_offset = ", end="", file=outfile)
    print(d_offset, end="", file=outfile)
    print(", ", file=outfile)
    print("  t_flight = ", end="", file=outfile)
    print(t_flight, file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    outputfile = open("output.txt", "w")
    print("s = ", end="", file=outputfile)
    print(s, file=outputfile)
    print("d_offset = ", end="", file=outputfile)
    print(d_offset, file=outputfile)
    print("t_flight = ", end="", file=outputfile)
    print(t_flight, file=outputfile)
    outputfile.close()

filename = sys.argv[1]
outfile = open("log.txt", "a")
print("var 'filename' assigned ", end="", file=outfile)
print(filename, end="", file=outfile)
print(" in module Projectile", file=outfile)
outfile.close()
inParams = InputParameters(filename)
t_flight = func_t_flight(inParams)
outfile = open("log.txt", "a")
print("var 't_flight' assigned ", end="", file=outfile)
print(t_flight, end="", file=outfile)
print(" in module Projectile", file=outfile)
outfile.close()
p_land = func_p_land(inParams)
outfile = open("log.txt", "a")
print("var 'p_land' assigned ", end="", file=outfile)
print(p_land, end="", file=outfile)
print(" in module Projectile", file=outfile)
outfile.close()
d_offset = func_d_offset(inParams, p_land)
outfile = open("log.txt", "a")
print("var 'd_offset' assigned ", end="", file=outfile)
print(d_offset, end="", file=outfile)
print(" in module Projectile", file=outfile)
outfile.close()
s = func_s(inParams, d_offset)
outfile = open("log.txt", "a")
print("var 's' assigned ", end="", file=outfile)
print(s, end="", file=outfile)
print(" in module Projectile", file=outfile)
outfile.close()
write_output(s, d_offset, t_flight)
