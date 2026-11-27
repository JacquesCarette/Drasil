module Drasil.SWHSNoPCM.Unitals (
  inputs, constrained, specParamValList, outputs
) where

import Language.Drasil

import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE

import Data.Drasil.SI_Units (centigrade)
import Data.Drasil.Quantities.Thermodynamics (temp)

import Drasil.SWHS.Unitals (absTol, arMax, arMin, coilHTC, coilHTCMax,
  coilHTCMin, coilSAUnc, coilSAMax, diamUnc, htCapW, htCapWMax, htCapWMin,
  lInit, relTol, tankLengthUnc, tankLengthMax, tankLengthMin, tempC, timeFinal,
  timeFinalMax, timeStep, wDensity, wDensityMax, wDensityMin, watE, tempW)

inputs :: NE.NonEmpty DefinedQuantityDict
inputs = (fmap dqdWr constrained <> unconstrained) `NE.prependList` NE.singleton watE

outputs :: NE.NonEmpty DefinedQuantityDict
outputs = tempW :| [watE]

unconstrained :: [DefinedQuantityDict]
unconstrained = [absTol, relTol]

constrained :: [UncertQ]
constrained = [coilSAUnc, htCapW, coilHTC, tempInit,
  timeFinal, tankLengthUnc, tempC, timeStep, wDensity, diamUnc]
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
