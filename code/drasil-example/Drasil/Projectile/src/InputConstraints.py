from __future__ import print_function
import sys
import math
import InputParameters


def input_constraints(inParams):
    if (not(0 < inParams.v)):
        raise Exception("InputError: Velocity must be positive.")
    if (not((0 < inParams.theta) and (inParams.theta < math.pi/2))):
        raise Exception("InputError: Angle must be between zero and pi over two radians.")
    if (not(0 < inParams.p_target)):
        raise Exception("InputError: Target position must be positive.")


