import load_params
import verify_params
import energy
import plot
import output
import verify_output
import sys
from scipy.integrate import ode
import numpy as np

filename = sys.argv[1]
filenamePrefix = ""
for char in filename:
    filenamePrefix = filenamePrefix + char
    if char is '.':
        break
params = load_params.load_params(filename)
verify_params.verify_valid(params)
verify_params.verify_recommended(params)

        
def f(t, Tw):
  return (1.0 / params.tau_w) * (params.Tc - Tw)
  
r = ode(f).set_integrator('vode', method='bdf', rtol=1e-8)
r.set_initial_value(params.Tinit)

t = [0.0]
Tw = [params.Tinit]

while r.successful() and r.t < params.tfinal:
  r.integrate(r.t + params.tstep)
  t.append(r.t)
  Tw.append(r.y[0])
  
Ew = energy.energyWat(Tw, params)  

verify_output.verify_output(t, Tw, Ew, params)
plot.plot(t, Tw, Ew, filenamePrefix)
output.output(params, t, Tw, Ew, Ew, filenamePrefix)
