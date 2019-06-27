from __future__ import print_function
import sys
import math

def input_constraints(A_C, C_W, h_C, T_init, t_final, L, T_C, t_step, rho_W, D, T_W, E_W):
    if (not((inParams.A_C <= 100000))) :
        print("Warning: constraint violated")
    if (not(((4170 < inParams.C_W) and (inParams.C_W < 4210)))) :
        print("Warning: constraint violated")
    if (not(((10 <= inParams.h_C) and (inParams.h_C <= 10000)))) :
        print("Warning: constraint violated")
    if (not((inParams.t_final < 86400))) :
        print("Warning: constraint violated")
    if (not(((0.1 <= inParams.L) and (inParams.L <= 50)))) :
        print("Warning: constraint violated")
    if (not(((950 < inParams.rho_W) and (inParams.rho_W <= 1000)))) :
        print("Warning: constraint violated")
    
    if (not((inParams.A_C > 0))) :
        print("Warning: constraint violated")
    if (not((inParams.C_W > 0))) :
        print("Warning: constraint violated")
    if (not((inParams.h_C > 0))) :
        print("Warning: constraint violated")
    if (not(((0 < inParams.T_init) and (inParams.T_init < 100)))) :
        print("Warning: constraint violated")
    if (not((inParams.t_final > 0))) :
        print("Warning: constraint violated")
    if (not((inParams.L > 0))) :
        print("Warning: constraint violated")
    if (not(((0 < inParams.T_C) and (inParams.T_C < 100)))) :
        print("Warning: constraint violated")
    if (not(((0 < inParams.t_step) and (inParams.t_step < inParams.t_final)))) :
        print("Warning: constraint violated")
    if (not((inParams.rho_W > 0))) :
        print("Warning: constraint violated")
    if (not((inParams.D > 0))) :
        print("Warning: constraint violated")
    if (not(((inParams.T_init <= inParams.T_W) and (inParams.T_W <= inParams.T_C)))) :
        print("Warning: constraint violated")
    if (not((inParams.E_W >= 0))) :
        print("Warning: constraint violated")


