"""
Input Format Module
Secret: The format of the input data.
Service: Converts the input data into the data structures used in the
input parameters module.
"""


def get_input(filename, params):
    """
    Reads the data from the input file and writes the data to the
    Param object.  Lines with comments are skipped.
    """

    infile = open(filename, "r")
    text = infile.readline()
    # dimensions of glass slab
    params.a = float(infile.readline())
    params.b = float(infile.readline())
    params.t = infile.readline()
    text = infile.readline()
    # glass type
    params.gt = infile.readline().rstrip()
    # Weight of Charge
    text = infile.readline()
    params.w = float(infile.readline())
    text = infile.readline()
    params.tnt = float(infile.readline())
    text = infile.readline()
    # stand off distance coordinates
    sdx = float(infile.readline())
    sdy = float(infile.readline())
    sdz = float(infile.readline())
    params.sdvect = (sdx, sdy, sdz)
    text = infile.readline()
    params.pbtol = float(infile.readline())
    infile.close()
