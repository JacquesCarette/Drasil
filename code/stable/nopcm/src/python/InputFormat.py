from __future__ import print_function
import sys
import math

def get_input(filename):
    lines = []
    linetokens = []
    infile = open(filename, "r")
    infile.readline()
    inParams.A_C = float(infile.readline())
    infile.readline()
    inParams.C_W = float(infile.readline())
    infile.readline()
    inParams.h_C = float(infile.readline())
    infile.readline()
    inParams.T_init = float(infile.readline())
    infile.readline()
    inParams.t_final = float(infile.readline())
    infile.readline()
    inParams.L = float(infile.readline())
    infile.readline()
    inParams.T_C = float(infile.readline())
    infile.readline()
    inParams.t_step = float(infile.readline())
    infile.readline()
    inParams.rho_W = float(infile.readline())
    infile.readline()
    inParams.D = float(infile.readline())
    infile.readline()
    inParams.A_tol = float(infile.readline())
    infile.readline()
    inParams.R_tol = float(infile.readline())
    infile.readline()
    inParams.T_W = float(infile.readline())
    infile.readline()
    inParams.E_W = float(infile.readline())
    infile.close()
    
    return A_C, C_W, h_C, T_init, t_final, L, T_C, t_step, rho_W, D, A_tol, R_tol, T_W, E_W


