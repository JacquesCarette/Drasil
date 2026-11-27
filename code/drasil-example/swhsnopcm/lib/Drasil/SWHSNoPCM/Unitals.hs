module Drasil.SWHSNoPCM.Unitals (
  inputs, inQuantities, constrained, specParamValList, outputs
) where

import Language.Drasil

import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE

import Drasil.SWHS.Unitals (absTol, arMax, arMin, coilHTC, coilHTCUnc,
  coilHTCMax, coilHTCMin, coilSA, coilSAUnc, coilSAMax, diam, diamUnc, htCapW,
  htCapWUnc, htCapWMax, htCapWMin, relTol, tankLength, tankLengthUnc,
  tankLengthMax, tankLengthMin, tempC, tempCUnc, tempInit, tempW, timeFinal,
  timeFinalUnc, timeFinalMax, timeStep, timeStepUnc, wDensity, wDensityUnc,
  wDensityMax, wDensityMin, watE)

inputs :: NE.NonEmpty DefinedQuantityDict
inputs = (inQuantities <> unconstrained) `NE.prependList` NE.singleton watE

outputs :: NE.NonEmpty DefinedQuantityDict
outputs = tempW :| [watE]

unconstrained :: [DefinedQuantityDict]
unconstrained = [absTol, relTol]

inQuantities :: [DefinedQuantityDict]
inQuantities = [coilSA, htCapW, coilHTC, tempInit,
  timeFinal, tankLength, tempC, timeStep, wDensity, diam]

constrained :: [UncertQ]
constrained = [coilSAUnc, htCapWUnc, coilHTCUnc, tempInitUnc,
  timeFinalUnc, tankLengthUnc, tempCUnc, timeStepUnc, wDensityUnc, diamUnc]
  -- w_E, temp_W

tempInitUnc :: UncertQ
tempInitUnc = uq (constrained' tempInit
  [physRange $ Bounded (Exc, exactDbl 0) (Exc, exactDbl 100)] (exactDbl 40)) defaultUncrt

specParamValList :: [ConstQDef]
specParamValList = [tankLengthMin, tankLengthMax,
  wDensityMin, wDensityMax, coilSAMax, htCapWMin, htCapWMax,
  coilHTCMin, coilHTCMax, timeFinalMax, arMin, arMax]
