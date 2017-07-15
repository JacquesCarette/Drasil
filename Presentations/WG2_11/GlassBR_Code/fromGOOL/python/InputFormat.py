from __future__ import print_function
import sys
import math
import InputParameters


def get_input(filename, inparams):
    infile = open(filename, "r")
    inparams.a = float(infile.readline());
    inparams.b = float(infile.readline());
    inparams.t = float(infile.readline());
    inparams.gt = int(infile.readline());
    inparams.w = float(infile.readline());
    inparams.tnt = float(infile.readline());
    inparams.sdx = float(infile.readline());
    inparams.sdy = float(infile.readline());
    inparams.sdz = float(infile.readline());
    inparams.pbtol = float(infile.readline());
    infile.close()


