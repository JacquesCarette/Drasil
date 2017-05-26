def linterp(time1, time2, result1, result2, desiredTime):
    desiredResult = result1 + (desiredTime - time1) / (time2 - time1) * (result2 - result1)
    return desiredResult
