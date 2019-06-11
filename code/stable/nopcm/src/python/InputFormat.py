from __future__ import print_function
import sys
import math

import InputParameters

def func_get_input(filename, inParams):
    lines = []
    linetokens = []
    infile = open(filename, "r")
    infile.readline()
    inParams.L = float(infile.readline())
    infile.readline()
    inParams.D = float(infile.readline())
    infile.readline()
    inParams.A_C = float(infile.readline())
    infile.readline()
    inParams.T_C = float(infile.readline())
    infile.readline()
    inParams.rho_W = float(infile.readline())
    infile.readline()
    inParams.C_W = float(infile.readline())
    infile.readline()
    inParams.h_C = float(infile.readline())
    infile.readline()
    inParams.T_init = float(infile.readline())
    infile.readline()
    inParams.t_step = float(infile.readline())
    infile.readline()
    inParams.t_final = float(infile.readline())
    infile.readline()
    inParams.A_tol = float(infile.readline())
    infile.readline()
    inParams.R_tol = float(infile.readline())
    infile.close()


