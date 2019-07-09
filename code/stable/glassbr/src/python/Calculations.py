from __future__ import print_function
import sys
import math

import InputParameters
import Interpolation

def func_J_tol(inParams):
    outfile = open("log.txt", "a")
    print("function func_J_tol called with inputs: {", file=outfile)
    print("  inParams = ", end='', file=outfile)
    print("Instance of InputParameters object", file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    return math.log((math.log((1 / (1 - inParams.P_btol))) * (((inParams.a * inParams.b) ** (7.0 - 1)) / (2.86e-53 * (((7.17e10 * (inParams.h ** 2)) ** 7.0) * inParams.LDF)))))

def func_q(inParams):
    outfile = open("log.txt", "a")
    print("function func_q called with inputs: {", file=outfile)
    print("  inParams = ", end='', file=outfile)
    print("Instance of InputParameters object", file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    return Interpolation.func_interpY("TSD.txt", inParams.SD, inParams.w_TNT)

def func_q_hat(inParams, q):
    outfile = open("log.txt", "a")
    print("function func_q_hat called with inputs: {", file=outfile)
    print("  inParams = ", end='', file=outfile)
    print("Instance of InputParameters object", end='', file=outfile)
    print(", ", file=outfile)
    print("  q = ", end='', file=outfile)
    print(q, file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    return ((q * ((inParams.a * inParams.b) ** 2)) / (7.17e10 * ((inParams.h ** 4) * inParams.GTF)))

def func_q_hat_tol(inParams, J_tol):
    outfile = open("log.txt", "a")
    print("function func_q_hat_tol called with inputs: {", file=outfile)
    print("  inParams = ", end='', file=outfile)
    print("Instance of InputParameters object", end='', file=outfile)
    print(", ", file=outfile)
    print("  J_tol = ", end='', file=outfile)
    print(J_tol, file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    return Interpolation.func_interpY("SDF.txt", inParams.AR, J_tol)

def func_J(inParams, q_hat):
    outfile = open("log.txt", "a")
    print("function func_J called with inputs: {", file=outfile)
    print("  inParams = ", end='', file=outfile)
    print("Instance of InputParameters object", end='', file=outfile)
    print(", ", file=outfile)
    print("  q_hat = ", end='', file=outfile)
    print(q_hat, file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    return Interpolation.func_interpZ("SDF.txt", inParams.AR, q_hat)

def func_NFL(inParams, q_hat_tol):
    outfile = open("log.txt", "a")
    print("function func_NFL called with inputs: {", file=outfile)
    print("  inParams = ", end='', file=outfile)
    print("Instance of InputParameters object", end='', file=outfile)
    print(", ", file=outfile)
    print("  q_hat_tol = ", end='', file=outfile)
    print(q_hat_tol, file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    return ((q_hat_tol * (7.17e10 * (inParams.h ** 4))) / ((inParams.a * inParams.b) ** 2))

def func_B(inParams, J):
    outfile = open("log.txt", "a")
    print("function func_B called with inputs: {", file=outfile)
    print("  inParams = ", end='', file=outfile)
    print("Instance of InputParameters object", end='', file=outfile)
    print(", ", file=outfile)
    print("  J = ", end='', file=outfile)
    print(J, file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    return ((2.86e-53 / ((inParams.a * inParams.b) ** (7.0 - 1))) * (((7.17e10 * (inParams.h ** 2)) ** 7.0) * (inParams.LDF * math.exp(J))))

def func_LR(inParams, NFL):
    outfile = open("log.txt", "a")
    print("function func_LR called with inputs: {", file=outfile)
    print("  inParams = ", end='', file=outfile)
    print("Instance of InputParameters object", end='', file=outfile)
    print(", ", file=outfile)
    print("  NFL = ", end='', file=outfile)
    print(NFL, file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    return (NFL * (inParams.GTF * 1))

def func_is_safeLR(LR, q):
    outfile = open("log.txt", "a")
    print("function func_is_safeLR called with inputs: {", file=outfile)
    print("  LR = ", end='', file=outfile)
    print(LR, end='', file=outfile)
    print(", ", file=outfile)
    print("  q = ", end='', file=outfile)
    print(q, file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    return (LR > q)

def func_P_b(B):
    outfile = open("log.txt", "a")
    print("function func_P_b called with inputs: {", file=outfile)
    print("  B = ", end='', file=outfile)
    print(B, file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    return (1 - math.exp(-(B)))

def func_is_safePb(inParams, P_b):
    outfile = open("log.txt", "a")
    print("function func_is_safePb called with inputs: {", file=outfile)
    print("  inParams = ", end='', file=outfile)
    print("Instance of InputParameters object", end='', file=outfile)
    print(", ", file=outfile)
    print("  P_b = ", end='', file=outfile)
    print(P_b, file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    return (P_b < inParams.P_btol)


