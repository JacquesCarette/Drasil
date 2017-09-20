import warnings

warnings.simplefilter('always', UserWarning)

def verify_output(t, Tw, Ew, params):
  previous = t[0]
  deltaTime = []
  for element in t[1:]:
      deltaTime = deltaTime + [element - previous]
      previous = element

  deltaEw = []
  prevWaterTemp = Tw[0]

  for timeStep, waterTemp in zip(deltaTime, Tw[1:]):
    deltaE = params.hc * params.Ac * timeStep * ((params.Tc - waterTemp) + (params.Tc - prevWaterTemp)) / 2
    deltaEw = deltaEw + [deltaE]
    prevWaterTemp = waterTemp

  Ew_tot = sum(deltaEw)
  error = abs(Ew_tot - Ew[-1]) / Ew[-1] * 100
  
  print(error)
  
  if error > params.ConsTol:
      warnings.warn('There is ' + str(error) + '% relative error ( > ' + str(params.ConsTol) + '% ) between the energy in the water output' +
                    ' and the expected output based on the law of conservation of energy.\n', UserWarning,
                    stacklevel=2)
