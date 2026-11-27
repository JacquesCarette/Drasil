module Drasil.SWHSNoPCM.Unitals (
  inputs, constrained, specParamValList, outputs
) where

import Language.Drasil

import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE

import Drasil.SWHS.Unitals (absTol, arMax, arMin, coilHTCUnc, coilHTCMax,
  coilHTCMin, coilSAUnc, coilSAMax, diamUnc, htCapWUnc, htCapWMax, htCapWMin,
  tempInit, relTol, tankLengthUnc, tankLengthMax, tankLengthMin, tempCUnc,
  timeFinal, timeFinalMax, timeStep, wDensityUnc, wDensityMax, wDensityMin,
  watE, tempW)

inputs :: NE.NonEmpty DefinedQuantityDict
inputs = (fmap dqdWr constrained <> unconstrained) `NE.prependList` NE.singleton watE

outputs :: NE.NonEmpty DefinedQuantityDict
outputs = tempW :| [watE]

unconstrained :: [DefinedQuantityDict]
unconstrained = [absTol, relTol]

constrained :: [UncertQ]
constrained = [coilSAUnc, htCapWUnc, coilHTCUnc, tempInitUnc,
  timeFinal, tankLengthUnc, tempCUnc, timeStep, wDensityUnc, diamUnc]
  -- w_E, temp_W

tempInitUnc :: UncertQ
tempInitUnc = uq (constrained' tempInit
  [physRange $ Bounded (Exc, exactDbl 0) (Exc, exactDbl 100)] (exactDbl 40)) defaultUncrt

specParamValList :: [ConstQDef]
specParamValList = [tankLengthMin, tankLengthMax,
  wDensityMin, wDensityMax, coilSAMax, htCapWMin, htCapWMax,
  coilHTCMin, coilHTCMax, timeFinalMax, arMin, arMax]
