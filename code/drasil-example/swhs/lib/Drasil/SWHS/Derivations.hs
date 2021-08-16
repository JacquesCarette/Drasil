module Drasil.SWHS.Derivations where

import Data.Drasil.Concepts.Documentation (assumption, condition, constraint,
  goal, input_, solution, output_)
import Data.Drasil.Concepts.Math (change, equation, ode, rightSide, rOfChng, surArea)
import Data.Drasil.Concepts.PhysicalProperties (liquid, mass, solid, vol)
import Data.Drasil.Concepts.Thermodynamics (boilPt, boiling, heat, heatCapSpec,
  heatTrans, htFlux, latentHeat, melting, phaseChange, sensHeat, temp)
import Data.Drasil.Quantities.Physics (energy, time)

import Drasil.SWHS.Assumptions (assumpCTNOD, assumpSITWP, assumpPIS, assumpWAL,
  assumpPIT, assumpNIHGBWP, assumpVCMPN, assumpNGSP, assumpAPT, assumpTHCCoL,
  assumpCWTAT, assumpTPCAV)
import Drasil.SWHS.Concepts (coil, phsChgMtrl, tank, water)
import Drasil.SWHS.DataDefs (ddHtFusion, ddMeltFrac, balanceDecayRate,
  balanceDecayTime, balanceSolidPCM, balanceLiquidPCM)
import Drasil.SWHS.GenDefs (htFluxWaterFromCoil, htFluxPCMFromWater, rocTempSimp)
import Drasil.SWHS.Goals (waterTempGS, pcmTempGS, waterEnergyGS, pcmEnergyGS)
import Drasil.SWHS.References (koothoor2013)
import Drasil.SWHS.TMods (sensHtE, latentHtE)
import Drasil.SWHS.Unitals (coilHTC, coilSA, eta, htFluxC, htFluxP, htCapLP,
  htCapSP, htCapW, htFusion, latentEP, meltFrac, pcmE, pcmHTC, pcmInitMltE,
  pcmMass, pcmSA, pcmVol, tInitMelt, tauLP, tauSP, tauW, tempC, tempInit,
  tempMeltP, tempPCM, tempW, timeFinal, volHtGen, watE, wMass, wVol)

import Language.Drasil.ModelExpr


--
eBalanceOnWtrDerivEqn1, eBalanceOnWtrDerivEqn2, eBalanceOnWtrDerivEqn3,
 eBalanceOnWtrDerivEqn4, eBalanceOnWtrDerivEqn5, eBalanceOnWtrDerivEqn6, eBalanceOnWtrDerivEqn7 :: ModelExpr

eBalanceOnWtrDerivEqn1 = sy wMass `mulRe` sy htCapW `mulRe` deriv (sy tempW) time $=
  sy htFluxC `mulRe` sy coilSA $- (sy htFluxP `mulRe` sy pcmSA)

eBalanceOnWtrDerivEqn2 = sy wMass `mulRe` sy htCapW `mulRe` deriv (sy tempW) time $=
  sy coilHTC `mulRe` sy coilSA `mulRe` (sy tempC $- sy tempW) $-
  (sy pcmHTC `mulRe` sy pcmSA `mulRe` (sy tempW $- sy tempPCM))

eBalanceOnWtrDerivEqn3 = deriv (sy tempW) time $=
  (sy coilHTC `mulRe` sy coilSA $/
  (sy wMass `mulRe` sy htCapW)) `mulRe` (sy tempC $- sy tempW) $-
  ((sy pcmHTC `mulRe` sy pcmSA $/
  (sy wMass `mulRe` sy htCapW)) `mulRe` (sy tempW $- sy tempPCM))

eBalanceOnWtrDerivEqn4 = deriv (sy tempW) time $=
  (sy coilHTC `mulRe` sy coilSA $/
  (sy wMass `mulRe` sy htCapW)) `mulRe`  (sy tempC $- sy tempW) `addRe`
  ((sy coilHTC `mulRe` sy coilSA $/
  (sy coilHTC `mulRe` sy coilSA)) `mulRe` (sy pcmHTC `mulRe` sy pcmSA $/
  (sy wMass `mulRe` sy htCapW)) `mulRe` (sy tempPCM $- sy tempW))

eBalanceOnWtrDerivEqn5 = deriv (sy tempW) time $=
  (sy coilHTC `mulRe` sy coilSA $/
  (sy wMass `mulRe` sy htCapW)) `mulRe`  (sy tempC $- sy tempW) `addRe`
  ((sy pcmHTC `mulRe` sy pcmSA $/
  (sy coilHTC `mulRe` sy coilSA)) `mulRe` (sy coilHTC `mulRe` sy coilSA $/
  (sy wMass `mulRe` sy htCapW)) `mulRe` (sy tempPCM $- sy tempW))

eBalanceOnWtrDerivEqn6 = deriv (sy tempW) time $=
  recip_ (sy tauW) `mulRe` (sy tempC $- sy tempW) `addRe` ((sy eta $/ sy tauW) `mulRe` (sy tempPCM $- sy tempW))

eBalanceOnWtrDerivEqn7 = deriv (sy tempW) time $=
  recip_ (sy tauW) `mulRe` ((sy tempC $- sy tempW) `addRe` (sy eta `mulRe` (sy tempPCM $- sy tempW)))

eBalanceOnWtrDerivEqnsIM1 :: [ModelExpr]
eBalanceOnWtrDerivEqnsIM1 = [eBalanceOnWtrDerivEqn1, eBalanceOnWtrDerivEqn2,
 eBalanceOnWtrDerivEqn3, eBalanceOnWtrDerivEqn4, eBalanceOnWtrDerivEqn5,
 eBalanceOnWtrDerivEqn6, eBalanceOnWtrDerivEqn7]

--
eBalanceOnPCMEqn1, eBalanceOnPCMEqn2, eBalanceOnPCMEqn3, eBalanceOnPCMEqn4 :: ModelExpr
eBalanceOnPCMEqn1 = sy pcmMass `mulRe` sy htCapSP `mulRe` deriv (sy tempPCM) time $=
  sy htFluxP `mulRe` sy pcmSA
eBalanceOnPCMEqn2 = sy pcmMass `mulRe` sy htCapSP `mulRe` deriv (sy tempPCM) time $=
  sy pcmHTC `mulRe` sy pcmSA `mulRe` (sy tempW $- sy tempPCM)
eBalanceOnPCMEqn3 = deriv (sy tempPCM) time $=
  ((sy pcmHTC `mulRe` sy pcmSA) $/ (sy pcmMass `mulRe` sy htCapSP)) `mulRe`  (sy tempW $- sy tempPCM)
eBalanceOnPCMEqn4 = deriv (sy tempPCM) time $=
  recip_ (sy tauSP) `mulRe` (sy tempW $- sy tempPCM)

eBalanceOnPCMDerivEqnsIM2 :: [ModelExpr]
eBalanceOnPCMDerivEqnsIM2 = [eBalanceOnPCMEqn1, eBalanceOnPCMEqn2, eBalanceOnPCMEqn3, eBalanceOnPCMEqn4]

