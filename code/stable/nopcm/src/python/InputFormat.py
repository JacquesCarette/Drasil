from __future__ import print_function
import sys
import math
import InputParameters


def func_get_inputs(filename, inParams, τ, A_tol, R_tol):
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
    inParams.ρ_W = float(infile.readline())
    infile.readline()
    inParams.C_W = float(infile.readline())
    infile.readline()
    inParams.h_C = float(infile.readline())
    infile.readline()
    inParams.T_init = float(infile.readline())
    infile.readline()
    τ = float(infile.readline())
    infile.readline()
    inParams.t_final = float(infile.readline())
    infile.readline()
    A_tol = float(infile.readline())
    infile.readline()
    R_tol = float(infile.readline())
    infile.readline()
    C_tol = float(infile.readline())
    infile.close()


