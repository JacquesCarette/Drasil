from __future__ import print_function
import sys
import math
import Interpolation
import InputParameters


def func_q(inParams):
    return Interpolation.func_interpY("TSD.txt", inParams.SD, inParams.w_TNT)

def func_is_safePb(inParams, P_b):
    return P_b < inParams.P_btol

def func_is_safeLR(LR, q):
    return LR > q

def func_B(inParams, J):
    return (2.86e-53 / ((inParams.a * inParams.b) ** (7.0 - 1))) * (((7.17e10 * (inParams.h ** 2)) ** 7.0) * (inParams.LDF * (math.exp(J))))

def func_J(inParams, q_hat):
    return Interpolation.func_interpZ("SDF.txt", inParams.AR, q_hat)

def func_NFL(inParams, q_hat_tol):
    return (q_hat_tol * (7.17e10 * (inParams.h ** 4))) / ((inParams.a * inParams.b) ** 2)

def func_q_hat(inParams, q):
    return (q * ((inParams.a * inParams.b) ** 2)) / (7.17e10 * ((inParams.h ** 4) * inParams.GTF))

def func_q_hat_tol(inParams, J_tol):
    return Interpolation.func_interpY("SDF.txt", inParams.AR, J_tol)

def func_J_tol(inParams):
    return math.log((math.log(1 / (1 - inParams.P_btol))) * (((inParams.a * inParams.b) ** (7.0 - 1)) / (2.86e-53 * (((7.17e10 * (inParams.h ** 2)) ** 7.0) * inParams.LDF))))

def func_P_b(B):
    return 1 - (math.exp(-(B)))

def func_LR(inParams, NFL):
    return NFL * (inParams.GTF * 1)

def func_q(inParams):
    return Interpolation.func_interpY("TSD.txt", inParams.SD, inParams.w_TNT)


