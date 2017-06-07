#Commented sections pending removal
import sys
sys.path.insert(0, '.')

import linterp


def PCM_ErrorF(Ffile, Pfile, comparator):
    f = open(Ffile, 'r')
    linesFor = f.readlines()
    timeFor = []
    tempWFor = []
    #tempPFor = []
    eWFor = []
    #ePFor = []
    tNoPCM = []
    eNoPCM = []
    for line in linesFor[23:]:
        values = line.split()
        timeFor += [float(values[0])]
        tempWFor += [float(values[2])]
        #tempPFor += [float(values[3])]
        eWFor += [float(values[5])]
        #ePFor += [float(values[4])]
        tNoPCM += [float(values[8])]
        eNoPCM += [float(values[9])]
    f.close()

    f = open(Pfile, 'r')
    linesPy = f.readlines()
    timePy = []
    tempWPy = []
    #tempPPy = []
    eWPy = []
    #ePPy = []
    for line in linesPy[34:]:
        values = line.split()
        timePy += [float(values[0])]
        tempWPy += [float(values[1])]
        #tempPPy += [float(values[2])]
        eWPy += [float(values[3])]
        #ePPy += [float(values[4])]
    f.close()

    if comparator is "TWat":
        error = errorCalcInterp(timeFor, timePy, tempWFor, tempWPy)
    #elif comparator is "TPCM":
        #error = errorCalcInterp(timeFor, timePy, tempPFor, tempPPy)
    elif comparator is "EWat":
        error = errorCalcInterp(timeFor, timePy, eWFor, eWPy)
    #elif comparator is "EPCM":
        #error = errorCalcInterp(timeFor, timePy, ePFor, ePPy)
    elif comparator is "TWatNoP":
        error = errorCalcInterp(timeFor, timePy, tNoPCM, tempWPy)
    elif comparator is "EWatNoP":
        error = errorCalcInterp(timeFor, timePy, eNoPCM, eWPy)
    else:
        error = 1
        
    print error
    return error


def PCM_ErrorM(Mfile, Pfile, comparator):
    f = open(Mfile, 'r')
    linesM = f.readlines()
    timeM = []
    tempWM = []
    #tempPM = []
    eWM = []
    #ePM = []
    for line in linesM[36:]:
        values = line.split()
        timeM += [float(values[0])]
        tempWM += [float(values[1])]
        #tempPM += [float(values[2])]
        eWM += [float(values[4])]
        #ePM += [float(values[3])]
    f.close()

    f = open(Pfile, 'r')
    linesPy = f.readlines()
    timePy = []
    tempWPy = []
    #tempPPy = []
    eWPy = []
    #ePPy = []
    for line in linesPy[34:]:
        values = line.split()
        timePy += [float(values[0])]
        tempWPy += [float(values[1])]
        #tempPPy += [float(values[2])]
        eWPy += [float(values[3])]
        #ePPy += [float(values[4])]
    f.close()

    if comparator is "TWat":
        error = errorCalcInterp(timeM, timePy, tempWM, tempWPy)
    #elif comparator is "TPCM":
        #error = errorCalcInterp(timeM, timePy, tempPM, tempPPy)
    elif comparator is "EWat":
        error = errorCalcInterp(timeM, timePy, eWM, eWPy)
    #elif comparator is "EPCM":
        #error = errorCalcInterp(timeM, timePy, ePM, ePPy)
    else:
        error = 1

    print error
    return error


def PCM_ErrorC(Cfile, Pfile, comparator):
    f = open(Cfile, 'r')
    linesC = f.readlines()
    timeC = []
    tempWC = []
    #tempPC = []
    eWC = []
    #ePC = []
    for line in linesC[34:]:
        values = line.split()
        timeC += [float(values[0])]
        tempWC += [float(values[1])]
        #tempPC += [float(values[2])]
        eWC += [float(values[3])]
        #ePC += [float(values[4])]
    f.close()

    f = open(Pfile, 'r')
    linesPy = f.readlines()
    timePy = []
    tempWPy = []
    #tempPPy = []
    eWPy = []
    #ePPy = []
    for line in linesPy[34:]:
        values = line.split()
        timePy += [float(values[0])]
        tempWPy += [float(values[1])]
        #tempPPy += [float(values[2])]
        eWPy += [float(values[3])]
        #ePPy += [float(values[4])]
    f.close()

    if comparator is "TWat":
        error = errorCalcInterp(timeC, timePy, tempWC, tempWPy)
    #elif comparator is "TPCM":
        #error = errorCalcInterp(timeC, timePy, tempPC, tempPPy)
    elif comparator is "EWat":
        error = errorCalcInterp(timeC, timePy, eWC, eWPy)
    #elif comparator is "EPCM":
        #error = errorCalcInterp(timeC, timePy, ePC, ePPy)
    else:
        error = 1

    print error
    return error

def errorCalcInterp(time1, time2, result1, result2):
    if len(time1) > len(time2):
        timePoints = time2
        interpTime = time1
        interpResult = result1
        stableResult = result2
    else:
        timePoints = time1
        interpTime = time2
        interpResult = result2
        stableResult = result1
    interpolated = [interpResult[0]]
    prevTime = interpTime[0]
    prevResult = interpResult[0]
    i = 1
    for time in timePoints[1:]:
        while i < len(interpTime):
            if interpTime[i] < time:
                prevTime = interpTime[i]
                prevResult = interpResult[i]
                i += 1
                continue
            else:
                interpolated += [linterp.linterp(prevTime, interpTime[i], prevResult, interpResult[i], time)]
                break
    delta = [abs(stableResult_i - interpolated_i) for stableResult_i, interpolated_i in zip(stableResult, interpolated)]
    deltaNorm = sum([delta_i ** 2 for delta_i in delta]) ** 0.5
    resultNorm = sum([stableResult_i ** 2 for stableResult_i in stableResult]) ** 0.5
    error = deltaNorm / resultNorm
    return error



