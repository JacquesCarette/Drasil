from __future__ import print_function
import sys
import math
import InputParameters


def calc_q_hat(q, inparams):
    q_hat = ((q * ((inparams.a * inparams.b) ** 2.0)) / (inparams.E * (inparams.h ** 4.0))) * (1.0 / inparams.gtf)
    return q_hat

def calc_j_tol(inparams):
    j_tol = math.log((math.log(1.0 / (1.0 - inparams.pbtol))) * ((((inparams.a / 1000.0) * (inparams.b / 1000.0)) ** (inparams.m - 1.0)) / ((inparams.k * (((inparams.E * 1000.0) * ((inparams.h / 1000.0) ** 2.0)) ** inparams.m)) * inparams.ldf)))
    return j_tol

def calc_pb(j, inparams):
    b = (((inparams.k / ((((inparams.a / 1000.0) * inparams.b) / 1000.0) ** (inparams.m - 1.0))) * (((1000.0 * inparams.E) * ((inparams.h / 1000.0) ** 2.0)) ** inparams.m)) * inparams.ldf) * (math.exp(j))
    pb = 1.0 - (math.exp(-(b)))
    return pb

def calc_nfl(q_hat_tol, inparams):
    nfl = ((q_hat_tol * inparams.E) * (inparams.h ** 4.0)) / ((inparams.a * inparams.b) ** 2.0)
    return nfl

def calc_lr(nfl, inparams):
    lr = (nfl * inparams.gtf) * inparams.lsf
    return lr

def calc_is_safe1(pb, inparams):
    if (pb < inparams.pbtol) :
        is_safe1 = True
    else :
        is_safe1 = False
    return is_safe1

def calc_is_safe2(lr, q):
    if (lr > q) :
        is_safe2 = True
    else :
        is_safe2 = False
    return is_safe2


