#Commented lines pending removal
def temperature1():
    rhs_Tw = '(1 / tau_w) * ((Tc - Tw))'# + eta * (Tp - Tw))'
#    rhs_Tp = '(1 / tau_ps) * (Tw - Tp)'

    odes = {'Tw': rhs_Tw}#, 'Tp': rhs_Tp}

    return odes


def temperature2():
    rhs_Tw = '(1 / tau_w) * ((Tc - Tw))'# + eta * (Tp - Tw))'
#    rhs_Tp = '0'
#    rhs_Qp = 'hp * Ap * (Tw - Tmelt)'

    odes = {'Tw': rhs_Tw}#, 'Tp': rhs_Tp, 'Qp': rhs_Qp}

    return odes


def temperature3():
    rhs_Tw = '(1 / tau_w) * ((Tc - Tw))'# + eta * (Tp - Tw))'
#    rhs_Tp = '(1 / tau_pl) * (Tw - Tp)'

    odes = {'Tw': rhs_Tw}#, 'Tp': rhs_Tp}

    return odes
