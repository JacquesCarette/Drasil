import math
import sys

def func_t_flight(v_launch, theta, g_vect):
    return 2 * v_launch * math.sin(theta) / g_vect

def func_p_land(v_launch, theta, g_vect):
    return 2 * v_launch ** 2 * math.sin(theta) * math.cos(theta) / g_vect

def func_d_offset(p_target, p_land):
    return p_land - p_target

def func_s(p_target, epsilon, d_offset):
    if (math.fabs(d_offset / p_target) < epsilon) :
        return "The target was hit."
    elif (d_offset < 0) :
        return "The projectile fell short."
    else :
        return "The projectile went long."

def get_input(filename):
    infile = open(filename, "r")
    infile.readline()
    v_launch = float(infile.readline())
    infile.readline()
    theta = float(infile.readline())
    infile.readline()
    p_target = float(infile.readline())
    infile.close()
    
    return v_launch, theta, p_target

def input_constraints(v_launch, theta, p_target):
    if (not(v_launch > 0)) :
        print("Warning: ", end='')
        print("v_launch has value ", end='')
        print(v_launch, end='')
        print(" but suggested to be ", end='')
        print("above ", end='')
        print(0, end='')
        print(".")
    if (not(0 < theta and theta < math.pi / 2)) :
        print("Warning: ", end='')
        print("theta has value ", end='')
        print(theta, end='')
        print(" but suggested to be ", end='')
        print("between ", end='')
        print(0, end='')
        print(" and ", end='')
        print(math.pi / 2, end='')
        print(" ((pi)/(2))", end='')
        print(".")
    if (not(p_target > 0)) :
        print("Warning: ", end='')
        print("p_target has value ", end='')
        print(p_target, end='')
        print(" but suggested to be ", end='')
        print("above ", end='')
        print(0, end='')
        print(".")

def write_output(s, d_offset):
    outputfile = open("output.txt", "w")
    print("s = ", end='', file=outputfile)
    print(s, file=outputfile)
    print("d_offset = ", end='', file=outputfile)
    print(d_offset, file=outputfile)
    outputfile.close()

filename = sys.argv[1]
g_vect = 9.8
epsilon = 2.0e-2
v_launch, theta, p_target = get_input(filename)
input_constraints(v_launch, theta, p_target)
t_flight = func_t_flight(v_launch, theta, g_vect)
p_land = func_p_land(v_launch, theta, g_vect)
d_offset = func_d_offset(p_target, p_land)
s = func_s(p_target, epsilon, d_offset)
write_output(s, d_offset)
