module Drasil.SWHSNoPCM.Unitals where

import Language.Drasil

import Data.Drasil.SI_Units (centigrade)
import Data.Drasil.Quantities.Thermodynamics (temp)

import Drasil.SWHS.Unitals (absTol, arMax, arMin, coilHTC, coilHTCMax,
  coilHTCMin, coilSA, coilSAMax, diam, htCapW, htCapWMax, htCapWMin, lInit,
  relTol, tankLength, tankLengthMax, tankLengthMin, tempC, timeFinal,
  timeFinalMax, timeStep, wDensity, wDensityMax, wDensityMin)

inputs :: [DefinedQuantityDict]
inputs = map dqdWr constrained ++ map dqdWr unconstrained

unconstrained :: [UncertQ]
unconstrained = [absTol, relTol]

constrained :: [UncertQ]
constrained =  [coilSA, htCapW, coilHTC, tempInit,
  timeFinal, tankLength, tempC, timeStep, wDensity, diam]
  -- w_E, temp_W

tempInit :: UncertQ
tempInit = uqc "tempInit" (nounPhraseSP "initial temperature")
  "the temperature at the beginning of the simulation"
  (sub (eqSymb temp) lInit) centigrade Real
  [physRange $ Bounded (Exc, exactDbl 0) (Exc, exactDbl 100)] (exactDbl 40) defaultUncrt

specParamValList :: [ConstQDef]
specParamValList = [tankLengthMin, tankLengthMax,
  wDensityMin, wDensityMax, coilSAMax, htCapWMin, htCapWMax, 
  coilHTCMin, coilHTCMax, timeFinalMax, arMin, arMax]

-- | Auto-generated internal variables for ODE solving
tempW_array, tempW_arrayvec, tempWd, tempWd_array :: DefinedQuantityDict

tempW_array = implVar
  "tempW_array"
  (nounPhraseSP "water temperature array")
  "auto-generated placeholder for ODE internal variable"
  (Array Real)
  (label "tempW_array")

tempW_arrayvec = implVar
  "tempW_arrayvec"
  (nounPhraseSP "water temperature array vector")
  "auto-generated placeholder for ODE internal variable"
  (Vect Real)
  (label "tempW_arrayvec")

tempWd = implVar
  "tempWd"
  (nounPhraseSP "water temperature derivative")
  "auto-generated placeholder for ODE internal variable"
  Real
  (label "tempWd")

tempWd_array = implVar
  "tempWd_array"
  (nounPhraseSP "water temperature derivative array")
  "auto-generated placeholder for ODE internal variable"
  (Array Real)
  (label "tempWd_array")

-- | Collect all manually defined ODE internal variables
collectODEInternalChunks :: [DefinedQuantityDict]
collectODEInternalChunks =
  [ tempW_array
  , tempW_arrayvec
  , tempWd
  , tempWd_array
  ]
