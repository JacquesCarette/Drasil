from __future__ import print_function
import sys
import math


def calc_B(k, a, b, m, E, h, LDF, J):
    return (((k / (((a / 1000) * (b / 1000)) ** (m - 1))) * (((E * 1000) * ((h / 1000) ** 2)) ** m)) * LDF) * (math.exp(J))

def calc_h(t):
    if (t == 2.5) :
        return 2.16
    elif (t == 2.7) :
        return 2.59
    elif (t == 3.0) :
        return 2.92
    elif (t == 4.0) :
        return 3.78
    elif (t == 5.0) :
        return 4.57
    elif (t == 6.0) :
        return 5.56
    elif (t == 8.0) :
        return 7.42
    elif (t == 10.0) :
        return 9.02
    elif (t == 12.0) :
        return 11.91
    elif (t == 16.0) :
        return 15.09
    elif (t == 19.0) :
        return 18.26
    elif (t == 22.0) :
        return 21.44

def calc_LDF(t_d, m):
    return (t_d / 60) ** (m / 16)

def calc_J(J, q_hat, a, b):
    return J(q_hat, a / b)

def calc_NFL(q_hat_tol, E, h, a, b):
    return ((q_hat_tol * E) * (h ** 4)) / ((a * b) ** 2)

def calc_GTF(g):
    if (g == "AN") :
        return 1
    elif (g == "FT") :
        return 4
    elif (g == "HS") :
        return 2

def calc_q_hat(q, a, b, E, h, GTF):
    return (q * ((a * b) ** 2)) / ((E * (h ** 4)) * GTF)

def calc_q_hat_tol(q_hat_tol, J_tol, a, b):
    return q_hat_tol(J_tol, a / b)

def calc_J_tol(P_btol, a, b, m, k, E, h, LDF):
    return math.log((math.log(1 / (1 - P_btol))) * ((((a / 1000) * (b / 1000)) ** (m - 1)) / ((k * (((E * 1000) * ((h / 1000) ** 2)) ** m)) * LDF)))


