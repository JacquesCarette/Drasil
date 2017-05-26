def energy1Wat(tWat, params):
    eWat = []
    for temp in tWat:
        eWat = eWat + [params.C_w * params.Mw * (temp - params.Tinit)]

    return eWat


def energy2Wat(tWat, params):
    eWat = []
    for temp in tWat:
        eWat = eWat + [params.C_w * params.Mw * (temp - params.Tinit)]

    return eWat


def energy3Wat(tWat, params):
    eWat = []
    for temp in tWat:
        eWat = eWat + [params.C_w * params.Mw * (temp - params.Tinit)]

    return eWat