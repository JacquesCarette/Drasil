## \file Calculations.py
# \author Nikitha Krithnan and W. Spencer Smith
# \brief Provides functions for calculating the outputs
import math

import Interpolation

## \brief Calculates minimum thickness (m)
# \param inParams structure holding the input values
# \return minimum thickness (m)
def func_h(inParams):
    outfile = open("log.txt", "a")
    print("function func_h called with inputs: {", file=outfile)
    print("  inParams = ", end="", file=outfile)
    print("Instance of InputParameters object", file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    return 1.0 / 1000.0 * (2.16 if inParams.t == 2.5 else 2.59 if inParams.t == 2.7 else 2.92 if inParams.t == 3.0 else 3.78 if inParams.t == 4.0 else 4.57 if inParams.t == 5.0 else 5.56 if inParams.t == 6.0 else 7.42 if inParams.t == 8.0 else 9.02 if inParams.t == 10.0 else 11.91 if inParams.t == 12.0 else 15.09 if inParams.t == 16.0 else 18.26 if inParams.t == 19.0 else 21.44)

## \brief Calculates glass type factor: a multiplying factor for adjusting the LR of different glass type, that is, AN, FT, or HS, in monolithic glass, LG (Laminated Glass), or IG (Insulating Glass) constructions
# \param inParams structure holding the input values
# \return glass type factor: a multiplying factor for adjusting the LR of different glass type, that is, AN, FT, or HS, in monolithic glass, LG (Laminated Glass), or IG (Insulating Glass) constructions
def func_GTF(inParams):
    outfile = open("log.txt", "a")
    print("function func_GTF called with inputs: {", file=outfile)
    print("  inParams = ", end="", file=outfile)
    print("Instance of InputParameters object", file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    if (inParams.g == "AN") :
        return 1
    elif (inParams.g == "FT") :
        return 4
    elif (inParams.g == "HS") :
        return 2
    else :
        raise Exception("Undefined case encountered in function func_GTF")

## \brief Calculates aspect ratio: the ratio of the long dimension of the glass to the short dimension of the glass. For glass supported on four sides, the aspect ratio is always equal to or greater than 1.0. For glass supported on three sides, the ratio of the length of one of the supported edges perpendicular to the free edge, to the length of the free edge, is equal to or greater than 0.5
# \param inParams structure holding the input values
# \return aspect ratio: the ratio of the long dimension of the glass to the short dimension of the glass. For glass supported on four sides, the aspect ratio is always equal to or greater than 1.0. For glass supported on three sides, the ratio of the length of one of the supported edges perpendicular to the free edge, to the length of the free edge, is equal to or greater than 0.5
def func_AR(inParams):
    outfile = open("log.txt", "a")
    print("function func_AR called with inputs: {", file=outfile)
    print("  inParams = ", end="", file=outfile)
    print("Instance of InputParameters object", file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    return inParams.a / inParams.b

## \brief Calculates applied load (demand): 3 second duration equivalent pressure (Pa)
# \param inParams structure holding the input values
# \return applied load (demand): 3 second duration equivalent pressure (Pa)
def func_q(inParams):
    outfile = open("log.txt", "a")
    print("function func_q called with inputs: {", file=outfile)
    print("  inParams = ", end="", file=outfile)
    print("Instance of InputParameters object", file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    return Interpolation.interpY("TSD.txt", inParams.SD, inParams.w_TNT)

## \brief Calculates dimensionless load
# \param inParams structure holding the input values
# \param q applied load (demand): 3 second duration equivalent pressure (Pa)
# \param h minimum thickness (m)
# \param GTF glass type factor: a multiplying factor for adjusting the LR of different glass type, that is, AN, FT, or HS, in monolithic glass, LG (Laminated Glass), or IG (Insulating Glass) constructions
# \return dimensionless load
def func_q_hat(inParams, q, h, GTF):
    outfile = open("log.txt", "a")
    print("function func_q_hat called with inputs: {", file=outfile)
    print("  inParams = ", end="", file=outfile)
    print("Instance of InputParameters object", end="", file=outfile)
    print(", ", file=outfile)
    print("  q = ", end="", file=outfile)
    print(q, end="", file=outfile)
    print(", ", file=outfile)
    print("  h = ", end="", file=outfile)
    print(h, end="", file=outfile)
    print(", ", file=outfile)
    print("  GTF = ", end="", file=outfile)
    print(GTF, file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    return q * (inParams.a * inParams.b) ** 2.0 / (7.17e10 * h ** 4.0 * GTF)

## \brief Calculates stress distribution factor (Function) based on Pbtol
# \param inParams structure holding the input values
# \param h minimum thickness (m)
# \return stress distribution factor (Function) based on Pbtol
def func_J_tol(inParams, h):
    outfile = open("log.txt", "a")
    print("function func_J_tol called with inputs: {", file=outfile)
    print("  inParams = ", end="", file=outfile)
    print("Instance of InputParameters object", end="", file=outfile)
    print(", ", file=outfile)
    print("  h = ", end="", file=outfile)
    print(h, file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    return math.log(math.log(1.0 / (1.0 - inParams.P_btol)) * ((inParams.a * inParams.b) ** (7.0 - 1.0) / (2.86e-53 * (7.17e10 * h ** 2.0) ** 7.0 * inParams.LDF)))

## \brief Calculates stress distribution factor (Function)
# \param AR aspect ratio: the ratio of the long dimension of the glass to the short dimension of the glass. For glass supported on four sides, the aspect ratio is always equal to or greater than 1.0. For glass supported on three sides, the ratio of the length of one of the supported edges perpendicular to the free edge, to the length of the free edge, is equal to or greater than 0.5
# \param q_hat dimensionless load
# \return stress distribution factor (Function)
def func_J(AR, q_hat):
    outfile = open("log.txt", "a")
    print("function func_J called with inputs: {", file=outfile)
    print("  AR = ", end="", file=outfile)
    print(AR, end="", file=outfile)
    print(", ", file=outfile)
    print("  q_hat = ", end="", file=outfile)
    print(q_hat, file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    return Interpolation.interpZ("SDF.txt", AR, q_hat)

## \brief Calculates tolerable load
# \param AR aspect ratio: the ratio of the long dimension of the glass to the short dimension of the glass. For glass supported on four sides, the aspect ratio is always equal to or greater than 1.0. For glass supported on three sides, the ratio of the length of one of the supported edges perpendicular to the free edge, to the length of the free edge, is equal to or greater than 0.5
# \param J_tol stress distribution factor (Function) based on Pbtol
# \return tolerable load
def func_q_hat_tol(AR, J_tol):
    outfile = open("log.txt", "a")
    print("function func_q_hat_tol called with inputs: {", file=outfile)
    print("  AR = ", end="", file=outfile)
    print(AR, end="", file=outfile)
    print(", ", file=outfile)
    print("  J_tol = ", end="", file=outfile)
    print(J_tol, file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    return Interpolation.interpY("SDF.txt", AR, J_tol)

## \brief Calculates risk of failure
# \param inParams structure holding the input values
# \param h minimum thickness (m)
# \param J stress distribution factor (Function)
# \return risk of failure
def func_B(inParams, h, J):
    outfile = open("log.txt", "a")
    print("function func_B called with inputs: {", file=outfile)
    print("  inParams = ", end="", file=outfile)
    print("Instance of InputParameters object", end="", file=outfile)
    print(", ", file=outfile)
    print("  h = ", end="", file=outfile)
    print(h, end="", file=outfile)
    print(", ", file=outfile)
    print("  J = ", end="", file=outfile)
    print(J, file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    return 2.86e-53 / (inParams.a * inParams.b) ** (7.0 - 1.0) * (7.17e10 * h ** 2.0) ** 7.0 * inParams.LDF * math.exp(J)

## \brief Calculates non-factored load: three second duration uniform load associated with a probability of breakage less than or equal to 8 lites per 1000 for monolithic AN glass (Pa)
# \param inParams structure holding the input values
# \param q_hat_tol tolerable load
# \param h minimum thickness (m)
# \return non-factored load: three second duration uniform load associated with a probability of breakage less than or equal to 8 lites per 1000 for monolithic AN glass (Pa)
def func_NFL(inParams, q_hat_tol, h):
    outfile = open("log.txt", "a")
    print("function func_NFL called with inputs: {", file=outfile)
    print("  inParams = ", end="", file=outfile)
    print("Instance of InputParameters object", end="", file=outfile)
    print(", ", file=outfile)
    print("  q_hat_tol = ", end="", file=outfile)
    print(q_hat_tol, end="", file=outfile)
    print(", ", file=outfile)
    print("  h = ", end="", file=outfile)
    print(h, file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    return q_hat_tol * 7.17e10 * h ** 4.0 / (inParams.a * inParams.b) ** 2.0

## \brief Calculates probability of breakage: the fraction of glass lites or plies that would break at the first occurrence of a specified load and duration, typically expressed in lites per 1000 (Ref: astm2016)
# \param B risk of failure
# \return probability of breakage: the fraction of glass lites or plies that would break at the first occurrence of a specified load and duration, typically expressed in lites per 1000 (Ref: astm2016)
def func_P_b(B):
    outfile = open("log.txt", "a")
    print("function func_P_b called with inputs: {", file=outfile)
    print("  B = ", end="", file=outfile)
    print(B, file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    return 1.0 - math.exp(-B)

## \brief Calculates load resistance: the uniform lateral load that a glass construction can sustain based upon a given probability of breakage and load duration as defined in (pp. 1 and 53) Ref: astm2009 (Pa)
# \param NFL non-factored load: three second duration uniform load associated with a probability of breakage less than or equal to 8 lites per 1000 for monolithic AN glass (Pa)
# \param GTF glass type factor: a multiplying factor for adjusting the LR of different glass type, that is, AN, FT, or HS, in monolithic glass, LG (Laminated Glass), or IG (Insulating Glass) constructions
# \return load resistance: the uniform lateral load that a glass construction can sustain based upon a given probability of breakage and load duration as defined in (pp. 1 and 53) Ref: astm2009 (Pa)
def func_LR(NFL, GTF):
    outfile = open("log.txt", "a")
    print("function func_LR called with inputs: {", file=outfile)
    print("  NFL = ", end="", file=outfile)
    print(NFL, end="", file=outfile)
    print(", ", file=outfile)
    print("  GTF = ", end="", file=outfile)
    print(GTF, file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    return NFL * GTF * 1.0

## \brief Calculates probability of glass breakage safety requirement
# \param inParams structure holding the input values
# \param P_b probability of breakage: the fraction of glass lites or plies that would break at the first occurrence of a specified load and duration, typically expressed in lites per 1000 (Ref: astm2016)
# \return probability of glass breakage safety requirement
def func_isSafePb(inParams, P_b):
    outfile = open("log.txt", "a")
    print("function func_isSafePb called with inputs: {", file=outfile)
    print("  inParams = ", end="", file=outfile)
    print("Instance of InputParameters object", end="", file=outfile)
    print(", ", file=outfile)
    print("  P_b = ", end="", file=outfile)
    print(P_b, file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    return P_b < inParams.P_btol

## \brief Calculates 3 second load equivalent resistance safety requirement
# \param LR load resistance: the uniform lateral load that a glass construction can sustain based upon a given probability of breakage and load duration as defined in (pp. 1 and 53) Ref: astm2009 (Pa)
# \param q applied load (demand): 3 second duration equivalent pressure (Pa)
# \return 3 second load equivalent resistance safety requirement
def func_isSafeLR(LR, q):
    outfile = open("log.txt", "a")
    print("function func_isSafeLR called with inputs: {", file=outfile)
    print("  LR = ", end="", file=outfile)
    print(LR, end="", file=outfile)
    print(", ", file=outfile)
    print("  q = ", end="", file=outfile)
    print(q, file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    return LR > q
