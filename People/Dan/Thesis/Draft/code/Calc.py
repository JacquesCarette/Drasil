## @file Calc.py
#  @brief Implements a series of functions to execute calculations
#  @date 07/27/2018

from Input import *
from ContoursADT import *
from math import log, exp

## @brief Calculates the Dimensionless load
#  @return the unitless load
def calc_q_hat( q, params ):
    upper = q * (params.a * params.b) ** 2
    lower = params.E * (params.h ** 4) * params.GTF
    return ( upper / lower )
## @brief Calculates the Stress distribution factor based on Pbtol
#  @return the unitless stress distribution factor
def calc_J_tol( params ):
    upper1 = 1
    lower1 = 1 - params.Pbtol

    upper2 = (params.a * params.b) ** (params.m - 1)
    lower2 = params.k * ((params.E * (params.h ** 2)) ** (params.m)) * params.LDF

    return (log( (log(upper1 / lower1)) * (upper2 / lower2) ))
## @brief Calculates the Probability of glass breakage
#  @return unitless probability of breakage
def calc_Pb( B ):
    output = 1 - (exp(-B))
    if not (0 < output < 1):
        raise InvalidOutput("Invalid output!")
    return (output)
## @brief Calculates the Risk of failure
#  @return unitless risk of failure
def calc_B( J, params ):
    upper = params.k * ((params.E * (params.h) ** 2) ** params.m) * params.LDF * exp(J)
    lower = ((params.a * params.b) ** (params.m - 1))
    return ( upper / lower )
## @brief Calculates the Non-factores load
#  @return unitless non-factored load
def calc_NFL( q̂tol, params ):
    upper = q̂tol * params.E * (params.h ** 4)
    lower = (params.a * params.b) ** 2
    return ( upper / lower )
## @brief Calculates the Load resistance
#  @return unitless load resistance
def calc_LR( NFL, params ):
    return ( NFL * params.GTF * params.LSF )
## @brief Calculates Safetey constraint 1
#  @return true if the calculated probability is less than the tolerable probability
def calc_is_safePb( Pb, params ):
    if (Pb < params.Pbtol):
        return True
    else:
        return False
## @brief Calculates Safetey constraint 2
#  @return true if the load resistance is greater than the load
def calc_is_safeLR( LR, q ):
    if (LR > q):
        return True
    else:
        return False
