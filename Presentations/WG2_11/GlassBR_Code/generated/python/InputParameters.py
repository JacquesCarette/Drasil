from __future__ import print_function
import sys
import math

a = 0.0
b = 0.0
w = 0.0
SD = 0.0
P_btol = 0.0
TNT = 0
g = ""
t = 0.0

def get_inputs(filename):
    infile = open(filename, "r")
    a = float(infile.readline());
    b = float(infile.readline());
    w = float(infile.readline());
    SD = float(infile.readline());
    P_btol = float(infile.readline());
    TNT = int(infile.readline());
    g = infile.readline();
    t = float(infile.readline());
    infile.close()

def input_constraints():
    if (not(d_min <= a)) :
        print("Warning: constraint violated");
    if (not(a <= d_max)) :
        print("Warning: constraint violated");
    if (not((a / b) < AR_max)) :
        print("Warning: constraint violated");
    if (not(d_min <= b)) :
        print("Warning: constraint violated");
    if (not(b <= d_max)) :
        print("Warning: constraint violated");
    if (not((a / b) < AR_max)) :
        print("Warning: constraint violated");
    if (not(w_max <= w)) :
        print("Warning: constraint violated");
    if (not(w <= w_min)) :
        print("Warning: constraint violated");
    if (not(SD_min < SD)) :
        print("Warning: constraint violated");
    if (not(SD < SD_max)) :
        print("Warning: constraint violated");
    if (not(a > 0.0)) :
        print("Warning: constraint violated");
    if (not((a / b) > 1.0)) :
        print("Warning: constraint violated");
    if (not(b > 0.0)) :
        print("Warning: constraint violated");
    if (not(b < a)) :
        print("Warning: constraint violated");
    if (not(w >= 0.0)) :
        print("Warning: constraint violated");
    if (not(SD > 0.0)) :
        print("Warning: constraint violated");
    if (not(0.0 < P_btol)) :
        print("Warning: constraint violated");
    if (not(P_btol < 1.0)) :
        print("Warning: constraint violated");
    if (not(TNT > 0.0)) :
        print("Warning: constraint violated");


