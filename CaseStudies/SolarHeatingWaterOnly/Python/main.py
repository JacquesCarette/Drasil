#Commented lines are ones pending removal

import load_params
import verify_params
import temperature
#import event
import energy
import plot
import output
import verify_output
import PyDSTool
from PyDSTool import args
import sys
from scipy import integrate

filename = sys.argv[1]
filenamePrefix = ""
for char in filename:
    filenamePrefix = filenamePrefix + char
    if char is '.':
        break
params = load_params.load_params(filename)
verify_params.verify_valid(params)
verify_params.verify_recommended(params)

pardict = {#'Ap': params.Ap,
#           'Tmelt': params.Tmelt,
#           'Hf': params.Hf,
           'Tc': params.Tc,
#           'hp': params.hp,
           'Tinit': params.Tinit,
           'tau_w': params.tau_w}#,
#           'eta': params.eta,
#           'Mp': params.Mp,
#           'tau_ps': params.tau_ps,
#           'tau_pl': params.tau_pl}

icdict1 = {'Tw': pardict['Tinit']}#,
#          'Tp': pardict['Tinit']}

DSargs1 = args(name='WaterTemp')
# DSargs1.events = [event.event1(params)]
DSargs1.pars = pardict
DSargs1.tdata = [0, params.tfinal]
DSargs1.algparams = {'init_step': params.tstep, 'rtol': params.RelTol, 'atol': params.AbsTol}
DSargs1.varspecs = temperature.temperature1()
DSargs1.ics = icdict1
waterTemp = PyDSTool.Generator.Vode_ODEsystem(DSargs1)

# def DSargs1(t, y):
    # return [y[1], -y[0]+sqrt(y[0])]]
# waterTemp =  ode(DSargs1).set_integrator('dopri5')
# traj1 = [0.0, params.Tinit]
# waterTemp.set_initial_value(traj1)

traj1 = waterTemp.compute('waterTemp')
pts1 = traj1.sample()

# evs1 = traj1.getEvents('event_melt_begin')
eWat = energy.energy1Wat(pts1['Tw'], params)
# ePCM = energy.energy1PCM(pts1['Tp'], params)
time = []
for element in pts1['t']:
    time = time + [element]
tempWat = []
for element in pts1['Tw']:
    tempWat = tempWat + [element]
#tempPCM = []
#for element in pts1['Tp']:
#    tempPCM = tempPCM + [element]

#if evs1 is None:
#    print("PCM has not started melting")
# else:
    # meltTime = evs1['t'][0]
    # print('PCM has started melting at time ' + str(meltTime))

    # icdict2 = {'Tw': pts1['Tw'][-1],
               # 'Tp': pts1['Tp'][-1],
               # 'Qp': 0}

    # DSargs2 = args(name='Melt')
    # DSargs2.events = [event.event2(params)]
    # DSargs2.pars = pardict
    # DSargs2.tdata = [pts1['t'][-1], params.tfinal]
    # if params.tfinal - pts1['t'][-1] < params.tstep:
        # initStep = params.tfinal - pts1['t'][-1]
    # else:
        # initStep = params.tstep
    # DSargs2.algparams = {'init_step': initStep, 'rtol': params.RelTol, 'atol': params.AbsTol}
    # DSargs2.varspecs = temperature.temperature2()
    # DSargs2.ics = icdict2
    # melt = PyDSTool.Generator.Vode_ODEsystem(DSargs2)
    # traj2 = melt.compute('temperature2')
    # pts2 = traj2.sample()
    # evs2 = traj2.getEvents('event_melt_end')
    # eWat = eWat + energy.energy2Wat(pts2['Tw'][1:], params)
    # ePCM = ePCM + energy.energy2PCM(pts2['Qp'][1:], params)
    # for element in pts2['t'][1:]:
        # time = time + [element]
    # for element in pts2['Tw'][1:]:
        # tempWat = tempWat + [element]
    # for element in pts2['Tp'][1:]:
        # tempPCM = tempPCM + [element]

    # if evs2 is None:
        # phi = pts2['Qp'][-1] / (params.Hf * params.Mp)
        # print(str(phi*100) + "% of the PCM has melted at time ", str(params.tfinal))
    # else:
        # meltEnd = evs2['t'][0]
        # print('PCM has finished melting at time ' + str(meltEnd))

        # icdict3 = {'Tw': pts2['Tw'][-1],
                   # 'Tp': pts2['Tp'][-1]}

        # DSargs3 = args(name='postMelt')
        # DSargs3.pars = pardict
        # DSargs3.tdata = [pts2['t'][-1], params.tfinal]
        # if params.tfinal - pts2['t'][-1] < params.tstep:
            # initStep = params.tfinal - pts2['t'][-1]
        # else:
            # initStep = params.tstep
        # DSargs3.algparams = {'init_step': initStep, 'rtol': params.RelTol, 'atol': params.AbsTol}
        # DSargs3.varspecs = temperature.temperature3()
        # DSargs3.ics = icdict3
        # postMelt = PyDSTool.Generator.Vode_ODEsystem(DSargs3)
        # traj3 = postMelt.compute('temperature2')
        # pts3 = traj3.sample()
        # eWat = eWat + energy.energy3Wat(pts3['Tw'][1:], params)
        # ePCM = ePCM + energy.energy3PCM(pts3['Tp'][1:], params)
        # for element in pts3['t'][1:]:
            # time = time + [element]
        # for element in pts3['Tw'][1:]:
            # tempWat = tempWat + [element]
        # for element in pts3['Tp'][1:]:
            # tempPCM = tempPCM + [element]

eTot = [sum(energies) for energies in zip(eWat)]#, ePCM)]

verify_output.verify_output(time, tempWat, eWat, params)#tempPCM, eWat, ePCM, params)
plot.plot(time, tempWat, eWat, filenamePrefix)#tempPCM, eWat, ePCM, filenamePrefix)
output.output(params, time, tempWat, eWat, eTot, filenamePrefix)#tempPCM, eWat, ePCM, eTot, filenamePrefix)
