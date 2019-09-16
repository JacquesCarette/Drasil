from __future__ import print_function
import sys
import math

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


