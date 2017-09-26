import math
import warnings

def verify_valid(params):
  if params.L <= 0:
      raise ValueError('Tank length must be > 0\n')
  elif params.diam <= 0:
      raise ValueError('Tank diameter must be > 0\n')
  elif params.Tc <= params.Tinit:
      raise ValueError('Tc must be > Tinit\n')
  elif params.Tc >= 100 or params.Tc <= 0:
      raise ValueError('Tc must be > 0 and < 100\n')
  elif params.Ac <= 0:
      raise ValueError('Ac must be > 0\n')
  elif params.rho_w <= 0:
      raise ValueError('rho_w must be > 0\n')
  elif params.C_w <= 0:
      raise ValueError('C_w must be > 0\n')
  elif params.hc <= 0:
      raise ValueError('hc must be > 0\n')
  elif params.Tinit <= 0 or params.Tinit >= 100:
      raise ValueError('Tinit must be > 0 and < 100\n')
  elif params.tfinal <= 0:
      raise ValueError('tfinal must be > 0\n')

def verify_recommended(params):
    
  warnings.simplefilter('always', UserWarning)
  
  if (params.L < 0.10) or (params.L > 50.0):
      warnings.warn('It is recommended that 0.1 <= L <= 50\n', UserWarning)
  if params.diam / params.L < 0.002 or params.diam / params.L > 200:
      warnings.warn('It is recommended that 0.002 <= D/L <= 200\n', UserWarning)
  if params.Ac > (math.pi * (params.diam / 2.0) ** 2.0):
      warnings.warn('It is recommended that Ac <= pi * (D/2) ^ 2\n', UserWarning)
  if params.rho_w <= 950.0 or params.rho_w > 1000.0:
      warnings.warn('It is recommended that 950 < rho_w <= 1000\n', UserWarning)
  if params.C_w <= 4170 or params.C_w >= 4210:
      warnings.warn('It is recommended that 4170 < C_w < 4210\n', UserWarning)
  if params.hc <= 10 or params.hc >= 10000:
      warnings.warn('It is recommended that 10 < hc < 10000\n', UserWarning)
  if params.tfinal <= 0 or params.tfinal >= 86400:
      warnings.warn('It is recommended that 0 < tfinal < 86400\n', UserWarning)
