from __future__ import print_function
import sys
import math
import InputParameters


def display_output(filename, q, j, q_hat_tol, pb, lr, nfl, is_safe1, is_safe2, inparams):
    outfile = open(filename, "w")
    print("a           ", end='', file=outfile);
    print(inparams.a, file=outfile);
    print("b           ", end='', file=outfile);
    print(inparams.b, file=outfile);
    print("t           ", end='', file=outfile);
    print(inparams.t, file=outfile);
    print("w           ", end='', file=outfile);
    print(inparams.w, file=outfile);
    print("tnt         ", end='', file=outfile);
    print(inparams.tnt, file=outfile);
    print("sdx         ", end='', file=outfile);
    print(inparams.sdx, file=outfile);
    print("sdy         ", end='', file=outfile);
    print(inparams.sdy, file=outfile);
    print("sdz         ", end='', file=outfile);
    print(inparams.sdz, file=outfile);
    print("pbtol       ", end='', file=outfile);
    print(inparams.pbtol, file=outfile);
    print("asprat      ", end='', file=outfile);
    print(inparams.asprat, file=outfile);
    print("sd          ", end='', file=outfile);
    print(inparams.sd, file=outfile);
    print("h           ", end='', file=outfile);
    print(inparams.h, file=outfile);
    print("gtf         ", end='', file=outfile);
    print(inparams.gtf, file=outfile);
    print("ldf         ", end='', file=outfile);
    print(inparams.ldf, file=outfile);
    print("wtnt        ", end='', file=outfile);
    print(inparams.wtnt, file=outfile);
    print("E           ", end='', file=outfile);
    print(inparams.E, file=outfile);
    print("td          ", end='', file=outfile);
    print(inparams.td, file=outfile);
    print("m           ", end='', file=outfile);
    print(inparams.m, file=outfile);
    print("k           ", end='', file=outfile);
    print(inparams.k, file=outfile);
    print("lsf         ", end='', file=outfile);
    print(inparams.lsf, file=outfile);
    print("gt          ", end='', file=outfile);
    print(inparams.gt, file=outfile);
    print("Demand (q)                      ", end='', file=outfile);
    print(q, file=outfile);
    print("Stress Distr. Factor (j)        ", end='', file=outfile);
    print(j, file=outfile);
    print("Tolerable Pressure (q_hat_tol)  ", end='', file=outfile);
    print(q_hat_tol, file=outfile);
    print("Prob. of Breakage (pb)          ", end='', file=outfile);
    print(pb, file=outfile);
    print("Capacity (lr)                   ", end='', file=outfile);
    print(lr, file=outfile);
    print("Non-Factored Load (nfl)         ", end='', file=outfile);
    print(nfl, file=outfile);
    print("Safety Req. 1 (is_safe1)        ", end='', file=outfile);
    print(is_safe1, file=outfile);
    print("Safety Req. 2 (is_safe2)        ", end='', file=outfile);
    print(is_safe2, file=outfile);
    if (is_safe1 and is_safe2) :
        print("For the given input parameters, the glass is considered safe.", file=outfile);
    else :
        print("For the given input parameters, the glass is NOT considered safe.", file=outfile);
    outfile.close()


