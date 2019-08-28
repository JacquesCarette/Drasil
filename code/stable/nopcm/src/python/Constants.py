## \file Constants.py
# \author Thulasi Jegatheesan
# \brief Provides the structure for holding constant values
from __future__ import print_function
import sys
import math

## \brief Structure for holding the constant values
class Constants:
    ## \brief Assigns values to variables for constants
    def __init__(self):
        self.L_min = 0.1
        self.L_max = 50
        self.rho_W_min = 950
        self.rho_W_max = 1000
        self.A_C_max = 100000
        self.C_W_min = 4170
        self.C_W_max = 4210
        self.h_C_min = 10
        self.h_C_max = 10000
        self.t_final_max = 86400
        self.AR_min = 1.0e-2
        self.AR_max = 100

