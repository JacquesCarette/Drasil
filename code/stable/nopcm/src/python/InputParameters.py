from __future__ import print_function
import sys
import math

A_C = 0.0
C_W = 0.0
h_C = 0.0
T_init = 0.0
t_final = 0.0
L = 0.0
T_C = 0.0
ρ_W = 0.0
D = 0.0
T_W = 0.0
E_W = 0.0

def derived_values():
    None

def input_constraints(inParams):
    if (not(inParams.A_C <= A_C_max)) :
        print("Warning: constraint violated")
    if (not((C_W_min < inParams.C_W) and (inParams.C_W < C_W_max))) :
        print("Warning: constraint violated")
    if (not((h_C_min <= inParams.h_C) and (inParams.h_C <= h_C_max))) :
        print("Warning: constraint violated")
    if (not(inParams.t_final < t_final_max)) :
        print("Warning: constraint violated")
    if (not((L_min <= inParams.L) and (inParams.L <= L_max))) :
        print("Warning: constraint violated")
    if (not((ρ_W_min < inParams.ρ_W) and (inParams.ρ_W <= ρ_W_max))) :
        print("Warning: constraint violated")
    if (not(inParams.A_C > 0)) :
        print("Warning: constraint violated")
    if (not(inParams.C_W > 0)) :
        print("Warning: constraint violated")
    if (not(inParams.h_C > 0)) :
        print("Warning: constraint violated")
    if (not((0 < inParams.T_init) and (inParams.T_init < 100))) :
        print("Warning: constraint violated")
    if (not(inParams.t_final > 0)) :
        print("Warning: constraint violated")
    if (not(inParams.L > 0)) :
        print("Warning: constraint violated")
    if (not((0 < inParams.T_C) and (inParams.T_C < 100))) :
        print("Warning: constraint violated")
    if (not(inParams.ρ_W > 0)) :
        print("Warning: constraint violated")
    if (not(inParams.D > 0)) :
        print("Warning: constraint violated")
    if (not((inParams.T_init <= inParams.T_W) and (inParams.T_W <= inParams.T_C))) :
        print("Warning: constraint violated")
    if (not(inParams.E_W >= 0)) :
        print("Warning: constraint violated")


