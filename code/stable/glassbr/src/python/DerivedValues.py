from __future__ import print_function
import sys
import math
import InputParameters


def derived_values(inParams):
    inParams.h = (1.0 / 1000.0) * (2.16 if (inParams.t == 2.5) else (2.59 if (inParams.t == 2.7) else (2.92 if (inParams.t == 3.0) else (3.78 if (inParams.t == 4.0) else (4.57 if (inParams.t == 5.0) else (5.56 if (inParams.t == 6.0) else (7.42 if (inParams.t == 8.0) else (9.02 if (inParams.t == 10.0) else (11.91 if (inParams.t == 12.0) else (15.09 if (inParams.t == 16.0) else (18.26 if (inParams.t == 19.0) else 21.44)))))))))))
    
    inParams.LDF = (3.0 / 60) ** (7.0 / 16)
    
    if (inParams.g == "AN") :
        inParams.GTF = 1
    elif (inParams.g == "FT") :
        inParams.GTF = 4
    elif (inParams.g == "HS") :
        inParams.GTF = 2
    
    inParams.SD = math.sqrt((inParams.SD_x ** 2) + ((inParams.SD_y ** 2) + (inParams.SD_z ** 2)))
    
    inParams.AR = inParams.a / inParams.b
    
    inParams.w_TNT = inParams.w * inParams.TNT


