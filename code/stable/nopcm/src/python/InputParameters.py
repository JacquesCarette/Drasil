from __future__ import print_function
import sys
import math

def input_constraints(A_C, C_W, h_C, T_init, t_final, L, T_C, t_step, rho_W, D, T_W, E_W):
    if (not((A_C <= 100000))) :
        print("Warning: constraint violated")
    if (not(((4170 < C_W) and (C_W < 4210)))) :
        print("Warning: constraint violated")
    if (not(((10 <= h_C) and (h_C <= 10000)))) :
        print("Warning: constraint violated")
    if (not((t_final < 86400))) :
        print("Warning: constraint violated")
    if (not(((0.1 <= L) and (L <= 50)))) :
        print("Warning: constraint violated")
    if (not(((950 < rho_W) and (rho_W <= 1000)))) :
        print("Warning: constraint violated")
    
    if (not((A_C > 0))) :
        print("Warning: constraint violated")
    if (not((C_W > 0))) :
        print("Warning: constraint violated")
    if (not((h_C > 0))) :
        print("Warning: constraint violated")
    if (not(((0 < T_init) and (T_init < 100)))) :
        print("Warning: constraint violated")
    if (not((t_final > 0))) :
        print("Warning: constraint violated")
    if (not((L > 0))) :
        print("Warning: constraint violated")
    if (not(((0 < T_C) and (T_C < 100)))) :
        print("Warning: constraint violated")
    if (not(((0 < t_step) and (t_step < t_final)))) :
        print("Warning: constraint violated")
    if (not((rho_W > 0))) :
        print("Warning: constraint violated")
    if (not((D > 0))) :
        print("Warning: constraint violated")
    if (not(((T_init <= T_W) and (T_W <= T_C)))) :
        print("Warning: constraint violated")
    if (not((E_W >= 0))) :
        print("Warning: constraint violated")


