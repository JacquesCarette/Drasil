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

    # parameters from input

    params.L = param[0]
    params.diam = param[1]
    params.Vp = param[2]
    params.Ap = param[3]
    params.rho_p = param[4]
    params.Tmelt = param[5]
    params.C_ps = param[6]
    params.C_pl = param[7]
    params.Hf = param[8]
    params.Ac = param[9]
    params.Tc = param[10]
    params.rho_w = param[11]
    params.C_w = param[12]
    params.hc = param[13]
    params.hp = param[14]
    params.Tinit = param[15]
    params.tstep = param[16]
    params.tfinal = param[17]
    params.AbsTol = param[18]
    params.RelTol = param[19]
    params.ConsTol = param[20]

    # calculated parameters

    params.Vt = math.pi * (params.diam / 2) ** 2 * params.L
    params.Mw = params.rho_w * (params.Vt - params.Vp)
    params.tau_w = (params.Mw * params.C_w) / (params.hc * params.Ac)
    params.eta = (params.hp * params.Ap) / (params.hc * params.Ac)
    params.Mp = params.rho_p * params.Vp
    params.tau_ps = (params.Mp * params.C_ps) / (params.hp * params.Ap)
    params.tau_pl = (params.Mp * params.C_pl) / (params.hp * params.Ap)
    params.Epmelt_init = params.C_ps * params.Mp * (params.Tmelt - params.Tinit)
    params.Ep_melt3 = params.Hf * params.Mp
    params.Mw_noPCM = params.rho_w * params.Vt
    params.tau_w_noPCM = (params.Mw_noPCM * params.C_w) / (params.hc * params.Ac)

    return params
