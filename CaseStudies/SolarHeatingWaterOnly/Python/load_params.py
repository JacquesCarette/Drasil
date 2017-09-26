import parameters
import math

def load_params(filename):
    f = open(filename, 'r')
    param = []
    for line in f:
        if line[0] is '#':
            continue
        else:
            param.append(float(line))
    f.close()
    params = parameters.Parameters()

    params.L = param[0]
    params.diam = param[1]
    params.Ac = param[2]
    params.Tc = param[3]
    params.rho_w = param[4]
    params.C_w = param[5]
    params.hc = param[6]
    params.Tinit = param[7]
    params.tstep = param[8]
    params.tfinal = param[9]
    params.AbsTol = param[10]
    params.RelTol = param[11]
    params.ConsTol = param[12]

    # calculated parameters

    params.Vt = math.pi * (params.diam / 2) ** 2 * params.L
    params.Mw = params.rho_w * (params.Vt)
    params.tau_w = (params.Mw * params.C_w) / (params.hc * params.Ac)

    return params
