def energyWat(tWat, params):
    eWat = []
    for temp in tWat:
        eWat = eWat + [params.C_w * params.Mw * (temp - params.Tinit)]

    return eWat