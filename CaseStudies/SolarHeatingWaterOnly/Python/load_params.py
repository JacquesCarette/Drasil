import parameters
import math

#Commented lines are ones that probably need removal but are awaiting verification

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
#    params.Vp = param[2]
#    params.Ap = param[3]
#    params.rho_p = param[4]
#    params.Tmelt = param[5]
#    params.C_ps = param[6]
#    params.C_pl = param[7]
#    params.Hf = param[2]
    params.Ac = param[2]
    params.Tc = param[3]
    params.rho_w = param[4]
    params.C_w = param[5]
    params.hc = param[6]
#    params.hp = param[14]
    params.Tinit = param[7]
    params.tstep = param[8]
    params.tfinal = param[9]
    params.AbsTol = param[10]
    params.RelTol = param[11]
    params.ConsTol = param[12]

    # calculated parameters

    params.Vt = math.pi * (params.diam / 2) ** 2 * params.L
    params.Mw = params.rho_w * (params.Vt)# - params.Vp)
    params.tau_w = (params.Mw * params.C_w) / (params.hc * params.Ac)
#    params.eta = (params.hp * params.Ap) / (params.hc * params.Ac)
#    params.Mp = params.rho_p * params.Vp
#    params.tau_ps = (params.Mp * params.C_ps) / (params.hp * params.Ap)
#    params.tau_pl = (params.Mp * params.C_pl) / (params.hp * params.Ap)
#    params.Epmelt_init = params.C_ps * params.Mp * (params.Tmelt - params.Tinit)
#    params.Ep_melt3 = params.Hf * params.Mp
    params.Mw_noPCM = params.rho_w * params.Vt
    params.tau_w_noPCM = (params.Mw_noPCM * params.C_w) / (params.hc * params.Ac)

    return params
