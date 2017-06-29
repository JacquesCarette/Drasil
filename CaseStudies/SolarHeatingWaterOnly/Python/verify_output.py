#Commented lines pending removal
import warnings

warnings.simplefilter('always', UserWarning)

def verify_output(time, tempW, eW, params):#tempP, eW, eP, params):
    previous = time[0]
    deltaTime = []
    for element in time[1:]:
        deltaTime = deltaTime + [element - previous]
        previous = element
    eCoil = []
#    ePCM = []
    eWater = []
    prevWaterTemp = tempW[0]
#    prevPCMTemp = tempP[0]
    for timeStep, waterTemp in zip(deltaTime, tempW[1:]): #pcmTemp in zip(deltaTime, tempW[1:], tempP[1:]):
        eCoilStep = params.hc * params.Ac * timeStep * (params.Tc - waterTemp + params.Tc - prevWaterTemp) / 2
#        ePCMStep = params.hp * params.Ap * timeStep * (waterTemp - pcmTemp + prevWaterTemp - prevPCMTemp) / 2
        eCoil = eCoil + [eCoilStep]
#        ePCM = ePCM + [ePCMStep]
        eWater = eWater + [eCoilStep]# - ePCMStep]
        prevWaterTemp = waterTemp
#        prevPCMTemp = pcmTemp

    eWaterTotal = sum(eWater)
#    ePCMTotal = sum(ePCM)

    errorWater = abs(eWaterTotal - eW[-1]) / eW[-1] * 100
#    errorPCM = abs(ePCMTotal - eP[-1]) / eP[-1] * 100
    
    if errorWater > params.ConsTol:
        warnings.warn('There is > ' + str(params.ConsTol) + '% relative error between the energy in the water output' +
                      ' and the expected output based on the law of conservation of energy.\n', UserWarning,
                      stacklevel=2)

#    if errorPCM > params.ConsTol:
#        warnings.warn('There is > ' + str(params.ConsTol) + '% relative error between the energy in the PCM output ' +
#                      'and the expected output based on the law of conservation of energy.\n', UserWarning,
#                      stacklevel=2)
