"""
Input Parameters Module
Secret: The format and structure of the input parameters.
Service: Stores the parameters needed for Glass-BR, including
the glass plate properties and the blast loading
"""

class Param(object):
    """
    Parameters for Glass-BR.

    State Variables
    a: plate length (long dimension) (mm)
    b: plate width (short dimension) (mm)
    t: nominal thickness (stored as a string) (mm)
    gt: glass type (stored as a string) (element of "AN", "HS", "GT")
    w: weight (kg)
    tnt: TNT equivalent factor
    sdvect: coordinates of stand off distance
    pbtol: tolerable pressure
    asprat: aspect ratio
    sd: stand off distance (m)
    h: actual thickness (mm)
    gtf: glass type factor
    wtnt: explosive mass in equivalent weight of TNT
    """

    def __init__(self):
        self.a = 0.0
        self.b = 0.0
        self.t = "2.5" #should use an enumerated type
        self.gt = "AN" #should use an enumerated type
        self.w = 0.0
        self.tnt = 0.0
        self.sdvect = (0.0, 0.0, 0.0)
        self.pbtol = 0.0

        self.asprat = 0.0
        self.sd = 0.0
        self.h = 0.0
        self.gtf = 0.0
        self.ldf = 0.0
        self.wtnt = 0.0

        # constant parameters
        self.E = 7.17*pow(10,7)
        self.td = 3
        self.m = 7
        self.k = 2.86*pow(10,-53)
        self.lsf = 1
