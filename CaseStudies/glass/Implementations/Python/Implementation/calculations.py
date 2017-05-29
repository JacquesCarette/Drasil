"""
Calculations Module
Secret: The equations for predicting the probability of glass breakage, capacity, and demand,
using the input parameters
Service: Converts the input data into the data structures used in the
input parameters module.
"""

from . import interp
import math
import numpy as np


def calc_q(w_array, data_sd, data_q, params):
    """
    Interpolates q (demand from IM3) from Figure 2 using wtnt and SD
    """
    idx, jdx, kdx, num_interp1, num_interp2 = interp.find_bounds(w_array, data_sd, params.wtnt, params.sd)
    q = interp.interp(idx, jdx, kdx, num_interp1, num_interp2, w_array, data_sd, data_q, params.wtnt, params.sd)
    return q


def calc_j(j_array, data_asprat, data_qstar, q, params):
    """
     Interpolates J (stress distribution factor from DD3) and q_hat_tol (tolerable pressure, DD7) from Figure 3 using
     the aspect ratio, Jtol (DD8) and q_hat (DD6).
     q* is interpolated for every J and stored in q_vec.  The corresponding J values are stored in j_vec.  J and
     q_hat_tol are interpolated from q_vec and j_vec using q_hat and Jtol.
    """
    q_hat = q * pow((params.a * params.b), 2) / (params.E * pow(params.h, 4)) * (1 / params.gtf)
    j_tol = math.log((math.log(1/(1-params.pbtol)))*((pow((params.a/1000)*(params.b/1000), params.m-1))/(params.k *
                                                (pow(params.E*1000*(pow((params.h/1000), 2)), params.m))*params.ldf)))
    q_vec = []
    j_vec = []
    for i in range(len(j_array)):
        idx_2 = i
        jdx_2 = (np.abs(data_asprat[:, idx_2] - params.asprat)).argmin()
        if data_asprat[jdx_2, idx_2] > params.asprat:
            jdx_2 -= 1
        if data_asprat[0, idx_2] > params.asprat:
            continue
        else:
            q_star = interp.interp(idx_2, jdx_2, -1, 0, 1, j_array, data_asprat, data_qstar, -1, params.asprat)
            q_vec.append(q_star)
            j_vec.append(j_array[idx_2])
    q_vec = np.array(q_vec)
    j_vec = np.array(j_vec)
    idx_3 = (np.abs(q_vec - q_hat)).argmin()
    if q_hat < np.amin(q_vec):
        raise SystemExit("Error: q_hat(DD6 in the SRS) is out of bounds. q_hat is less than the smallest value in q_vec. The input a, b might be too small while t might be too large. Please refer to the data definitions section and the data constraints section in the SRS.")
    if q_hat > np.amax(q_vec):
        raise SystemExit("Error: q_hat(DD6 in SRS) is out of bounds. q_hat is greater than the biggest value in q_vec. The input a, b might be too large while t might be too small. Please refer to the data definitions section and the data constraints section in the SRS.")
    else:
        if q_vec[idx_3] > q_hat:
            idx_3 -= 1
        j = interp.lin_interp(j_vec[idx_3], j_vec[idx_3+1], q_vec[idx_3], q_vec[idx_3+1], q_hat)
        idx_4 = (np.abs(j_vec - j_tol)).argmin()
        if j_vec[idx_4] > j_tol:
            idx_4 -= 1
        q_hat_tol = interp.lin_interp(q_vec[idx_4], q_vec[idx_4+1], j_vec[idx_4], j_vec[idx_4+1], j_tol)
        return j, q_hat_tol


def calc_pb(j, params):
    """
    Calculates probability of glass breakage (Pb, IM1).
    """
    b = (params.k / (pow(params.a/1000 * params.b/1000, params.m - 1))) * (
        pow(1000*params.E * pow(params.h/1000, 2), params.m)) * params.ldf * (math.exp(j))
    pb = 1 - math.exp(-b)
    return pb


def calc_lr(q_hat_tol, params):
    """
    Calculates Capacity (LR, IM2).
    """
    nfl = (q_hat_tol * params.E * pow(params.h, 4)) / (pow(params.a * params.b, 2))
    lr = nfl * params.gtf * params.lsf
    return lr, nfl


def is_safe(pb, lr, q, params):
    """
    Checks if Pb < Pbtol and if LR > q (T1 and T2).
    If both are true, the glass is considered safe.
    """
    if pb < params.pbtol:
        is_safe1 = True
    else:
        is_safe1 = False
    if lr > q:
        is_safe2 = True
    else:
        is_safe2 = False
    if is_safe1 == True and is_safe2 == True:
        safe = 'For the given input parameters, the glass is considered safe'
    else:
        safe = 'For the given input parameters, the glass is NOT considered safe'
    return is_safe1, is_safe2, safe
