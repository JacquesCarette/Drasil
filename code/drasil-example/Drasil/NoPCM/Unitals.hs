module Drasil.NoPCM.Unitals where

import Language.Drasil

import Data.Drasil.SI_Units (centigrade)
import Data.Drasil.Quantities.Thermodynamics (temp)

import Drasil.SWHS.Unitals (absTol, arMax, arMin, coilHTC, coilHTCMax,
  coilHTCMin, coilSA, coilSAMax, diam, htCapW, htCapWMax, htCapWMin, lInit,
  relTol, tankLength, tankLengthMax, tankLengthMin, tempC, timeFinal,
  timeFinalMax, timeStep, wDensity, wDensityMax, wDensityMin)

inputs :: [QuantityDict]
inputs = map qw constrained ++ map qw unconstrained

unconstrained :: [UncertainChunk]
unconstrained = [absTol, relTol]

constrained :: [UncertQ]
constrained =  [coilSA, htCapW, coilHTC, tempInit,
  timeFinal, tankLength, tempC, timeStep, wDensity, diam]
  -- w_E, temp_W

tempInit :: UncertQ
tempInit = uqc "tempInit" (nounPhraseSP "initial temperature")
  "The temperature at the beginning of the simulation"
  (sub (eqSymb temp) lInit) centigrade Real
  [physc $ Bounded (Exc,0) (Exc,100)] (dbl 40) defaultUncrt

specParamValList :: [QDefinition]
specParamValList = [tankLengthMin, tankLengthMax,
  wDensityMin, wDensityMax, coilSAMax, htCapWMin, htCapWMax, 
  coilHTCMin, coilHTCMax, timeFinalMax, arMin, arMax]
