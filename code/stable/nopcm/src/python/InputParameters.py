## \file InputParameters.py
# \brief Provides the function for reading inputs and the function for checking the physical constraints and software constraints on the input
from __future__ import print_function
import sys
import math

## \brief Reads input from a file with the given file name
# \param filename name of the input file
def get_input(filename):
    infile = open(filename, "r")
    infile.readline()
    A_C = float(infile.readline())
    infile.readline()
    C_W = float(infile.readline())
    infile.readline()
    h_C = float(infile.readline())
    infile.readline()
    T_init = float(infile.readline())
    infile.readline()
    t_final = float(infile.readline())
    infile.readline()
    L = float(infile.readline())
    infile.readline()
    T_C = float(infile.readline())
    infile.readline()
    t_step = float(infile.readline())
    infile.readline()
    rho_W = float(infile.readline())
    infile.readline()
    D = float(infile.readline())
    infile.readline()
    A_tol = float(infile.readline())
    infile.readline()
    R_tol = float(infile.readline())
    infile.readline()
    T_W = float(infile.readline())
    infile.readline()
    E_W = float(infile.readline())
    infile.close()
    
    return A_C, C_W, h_C, T_init, t_final, L, T_C, t_step, rho_W, D, A_tol, R_tol, T_W, E_W

## \brief Verifies that input values satisfy the physical constraints and software constraints
# \param A_C heating coil surface area
# \param C_W specific heat capacity of water
# \param h_C convective heat transfer coefficient between coil and water
# \param T_init initial temperature
# \param t_final final time
# \param L length of tank
# \param T_C temperature of the heating coil
# \param t_step time step for simulation
# \param rho_W density of water
# \param D diameter of tank
# \param T_W temperature of the water
# \param E_W change in heat energy in the water
def input_constraints(A_C, C_W, h_C, T_init, t_final, L, T_C, t_step, rho_W, D, T_W, E_W):
    if (not(A_C <= 100000)) :
        print("Warning: constraint violated")
    if (not(4170 < C_W and C_W < 4210)) :
        print("Warning: constraint violated")
    if (not(10 <= h_C and h_C <= 10000)) :
        print("Warning: constraint violated")
    if (not(t_final < 86400)) :
        print("Warning: constraint violated")
    if (not(0.1 <= L and L <= 50)) :
        print("Warning: constraint violated")
    if (not(950 < rho_W and rho_W <= 1000)) :
        print("Warning: constraint violated")
    
    if (not(A_C > 0)) :
        print("Warning: constraint violated")
    if (not(C_W > 0)) :
        print("Warning: constraint violated")
    if (not(h_C > 0)) :
        print("Warning: constraint violated")
    if (not(0 < T_init and T_init < 100)) :
        print("Warning: constraint violated")
    if (not(t_final > 0)) :
        print("Warning: constraint violated")
    if (not(L > 0)) :
        print("Warning: constraint violated")
    if (not(0 < T_C and T_C < 100)) :
        print("Warning: constraint violated")
    if (not(0 < t_step and t_step < t_final)) :
        print("Warning: constraint violated")
    if (not(rho_W > 0)) :
        print("Warning: constraint violated")
    if (not(D > 0)) :
        print("Warning: constraint violated")
    if (not(T_init <= T_W and T_W <= T_C)) :
        print("Warning: constraint violated")
    if (not(E_W >= 0)) :
        print("Warning: constraint violated")


