"""
Calculations Module
Secret: The equations for predicting the probability of glass breakage, capacity, and demand,
using the input parameters
Service: Converts the input data into the data structures used in the
input parameters module.
"""

from . import interp
import math

    
def calc_q_hat(q, params):
    q_hat = q * pow((params.a * params.b), 2) / (params.E * pow(params.h, 4)) * (1 / params.gtf)
    return q_hat
    
def calc_j_tol(params):
    j_tol = math.log((math.log(1/(1-params.pbtol)))*((pow((params.a/1000)*(params.b/1000), params.m-1))/(params.k *(pow(params.E*1000*(pow((params.h/1000), 2)), params.m))*params.ldf)))
    return j_tol                                         
   

def calc_pb(j, params):
    """
    Calculates probability of glass breakage (Pb, IM1).
    """
    b = (params.k / (pow(params.a/1000 * params.b/1000, params.m - 1))) * (
        pow(1000*params.E * pow(params.h/1000, 2), params.m)) * params.ldf * (math.exp(j))
    pb = 1 - math.exp(-b)
    return pb


def calc_nfl(q_hat_tol, params):
    nfl = (q_hat_tol * params.E * pow(params.h, 4)) / (pow(params.a * params.b, 2))
    return nfl
    
def calc_lr(nfl, params):
    lr = nfl * params.gtf * params.lsf
    return lr

def is_safe1(pb, params):
    if pb < params.pbtol:
        is_safe1 = True
    else:
        is_safe1 = False
    return is_safe1
    
def is_safe2(lr, q):
    if lr > q:
        is_safe2 = True
    else:
        is_safe2 = False
    return is_safe2
