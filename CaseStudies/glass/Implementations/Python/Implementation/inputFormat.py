"""
Input Format Module
Secret: The format of the input data.
Service: Converts the input data into the data structures used in the
input parameters module.
"""

from numpy import float64


def get_input(filename, params):
    """
    Reads the data from the input file and writes the data to the
    Param object.  Lines with comments are skipped.
    """

    infile = open(filename, "r")
    text = infile.readline()
    # dimensions of glass slab
    params.a = float64(infile.readline())
    params.b = float64(infile.readline())
    params.t = infile.readline()
    text = infile.readline()
    # glass type
    params.gt = infile.readline().rstrip()
    # Weight of Charge
    text = infile.readline()
    params.w = float64(infile.readline())
    text = infile.readline()
    params.tnt = float64(infile.readline())
    text = infile.readline()
    # stand off distance coordinates
    sdx = float64(infile.readline())
    sdy = float64(infile.readline())
    sdz = float64(infile.readline())
    params.sdvect = (sdx, sdy, sdz)
    text = infile.readline()
    params.pbtol = float64(infile.readline())
    infile.close()
