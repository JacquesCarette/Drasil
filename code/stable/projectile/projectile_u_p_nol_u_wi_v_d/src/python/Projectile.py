## \file Projectile.py
# \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
# \brief Contains the entire Projectile program
import math
import sys

## \brief Calculates flight duration: the time when the projectile lands (s)
# \param v_launch launch speed: the initial speed of the projectile when launched (m/s)
# \param theta launch angle: the angle between the launcher and a straight line from the launcher to the target (rad)
# \param g_vect gravitational acceleration (m/s^2)
# \return flight duration: the time when the projectile lands (s)
def func_t_flight(v_launch, theta, g_vect):
    return 2.0 * v_launch * math.sin(theta) / g_vect

## \brief Calculates landing position: the distance from the launcher to the final position of the projectile (m)
# \param v_launch launch speed: the initial speed of the projectile when launched (m/s)
# \param theta launch angle: the angle between the launcher and a straight line from the launcher to the target (rad)
# \param g_vect gravitational acceleration (m/s^2)
# \return landing position: the distance from the launcher to the final position of the projectile (m)
def func_p_land(v_launch, theta, g_vect):
    return 2.0 * v_launch ** 2.0 * math.sin(theta) * math.cos(theta) / g_vect

## \brief Calculates distance between the target position and the landing position: the offset between the target position and the landing position (m)
# \param p_target target position: the distance from the launcher to the target (m)
# \param p_land landing position: the distance from the launcher to the final position of the projectile (m)
# \return distance between the target position and the landing position: the offset between the target position and the landing position (m)
def func_d_offset(p_target, p_land):
    return p_land - p_target

## \brief Calculates output message as a string
# \param p_target target position: the distance from the launcher to the target (m)
# \param epsilon hit tolerance
# \param d_offset distance between the target position and the landing position: the offset between the target position and the landing position (m)
# \return output message as a string
def func_s(p_target, epsilon, d_offset):
    if (math.fabs(d_offset / p_target) < epsilon) :
        return "The target was hit."
    elif (d_offset < 0.0) :
        return "The projectile fell short."
    else :
        return "The projectile went long."

## \brief Reads input from a file with the given file name
# \param filename name of the input file
# \return launch speed: the initial speed of the projectile when launched (m/s)
# \return launch angle: the angle between the launcher and a straight line from the launcher to the target (rad)
# \return target position: the distance from the launcher to the target (m)
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

## \brief Verifies that input values satisfy the physical constraints
# \param v_launch launch speed: the initial speed of the projectile when launched (m/s)
# \param theta launch angle: the angle between the launcher and a straight line from the launcher to the target (rad)
# \param p_target target position: the distance from the launcher to the target (m)
def input_constraints(v_launch, theta, p_target):
    if (not(v_launch > 0.0)) :
        print("Warning: ", end="")
        print("v_launch has value ", end="")
        print(v_launch, end="")
        print(", but is suggested to be ", end="")
        print("above ", end="")
        print(0.0, end="")
        print(".")
    if (not(0.0 < theta and theta < math.pi / 2.0)) :
        print("Warning: ", end="")
        print("theta has value ", end="")
        print(theta, end="")
        print(", but is suggested to be ", end="")
        print("between ", end="")
        print(0.0, end="")
        print(" and ", end="")
        print(math.pi / 2.0, end="")
        print(" ((pi)/(2))", end="")
        print(".")
    if (not(p_target > 0.0)) :
        print("Warning: ", end="")
        print("p_target has value ", end="")
        print(p_target, end="")
        print(", but is suggested to be ", end="")
        print("above ", end="")
        print(0.0, end="")
        print(".")

## \brief Writes the output values to output.txt
# \param s output message as a string
# \param d_offset distance between the target position and the landing position: the offset between the target position and the landing position (m)
# \param t_flight flight duration: the time when the projectile lands (s)
def write_output(s, d_offset, t_flight):
    outputfile = open("output.txt", "w")
    print("s = ", end="", file=outputfile)
    print(s, file=outputfile)
    print("d_offset = ", end="", file=outputfile)
    print(d_offset, file=outputfile)
    print("t_flight = ", end="", file=outputfile)
    print(t_flight, file=outputfile)
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
write_output(s, d_offset, t_flight)
