def energy1Wat(tWat, params):
    eWat = []
    for temp in tWat:
        eWat = eWat + [params.C_w * params.Mw * (temp - params.Tinit)]

    return eWat


def energy1PCM(tPCM, params):
    ePCM = []
    for temp in tPCM:
        ePCM = ePCM + [params.C_ps * params.Mp * (temp - params.Tinit)]

    return ePCM


def energy2Wat(tWat, params):
    eWat = []
    for temp in tWat:
        eWat = eWat + [params.C_w * params.Mw * (temp - params.Tinit)]

    return eWat


def energy2PCM(Qp, params):
    ePCM = []
    for latHeat in Qp:
        ePCM = ePCM + [params.Epmelt_init + latHeat]

    return ePCM


def energy3Wat(tWat, params):
    eWat = []
    for temp in tWat:
        eWat = eWat + [params.C_w * params.Mw * (temp - params.Tinit)]

    return eWat


def energy3PCM(tPCM, params):
    ePCM = []
    for temp in tPCM:
        ePCM = ePCM + [params.Epmelt_init + params.Ep_melt3 + params.C_pl * params.Mp * (temp - params.Tmelt)]

    return ePCM
