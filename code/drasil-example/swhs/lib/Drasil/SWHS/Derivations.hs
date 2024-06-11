module Drasil.SWHS.Derivations where

import Data.Drasil.Quantities.Physics (time)

import Drasil.SWHS.Unitals (wMass, tauW, tauSP, pcmSA, pcmMass,
  pcmHTC, htCapW, htCapSP, eta, coilSA, coilHTC, htFluxC,
  htFluxP, tempC, tempW, tempPCM)

import Language.Drasil (ModelExprC(deriv), ExprC(..), ModelExpr, recip_)

--
eBalanceOnWtrDerivEqn1, eBalanceOnWtrDerivEqn2, eBalanceOnWtrDerivEqn3,
 eBalanceOnWtrDerivEqn4, eBalanceOnWtrDerivEqn5, eBalanceOnWtrDerivEqn6, eBalanceOnWtrDerivEqn7 :: ModelExpr

eBalanceOnWtrDerivEqn1 = sy wMass `mul` sy htCapW `mul` deriv (sy tempW) time $=
  sy htFluxC `mul` sy coilSA $- (sy htFluxP `mul` sy pcmSA)

eBalanceOnWtrDerivEqn2 = sy wMass `mul` sy htCapW `mul` deriv (sy tempW) time $=
  sy coilHTC `mul` sy coilSA `mul` (sy tempC $- sy tempW) $-
  (sy pcmHTC `mul` sy pcmSA `mul` (sy tempW $- sy tempPCM))

eBalanceOnWtrDerivEqn3 = deriv (sy tempW) time $=
  (sy coilHTC `mul` sy coilSA $/
  (sy wMass `mul` sy htCapW)) `mul` (sy tempC $- sy tempW) $-
  ((sy pcmHTC `mul` sy pcmSA $/
  (sy wMass `mul` sy htCapW)) `mul` (sy tempW $- sy tempPCM))

eBalanceOnWtrDerivEqn4 = deriv (sy tempW) time $=
  (sy coilHTC `mul` sy coilSA $/
  (sy wMass `mul` sy htCapW)) `mul`  (sy tempC $- sy tempW) `add`
  ((sy coilHTC `mul` sy coilSA $/
  (sy coilHTC `mul` sy coilSA)) `mul` (sy pcmHTC `mul` sy pcmSA $/
  (sy wMass `mul` sy htCapW)) `mul` (sy tempPCM $- sy tempW))

eBalanceOnWtrDerivEqn5 = deriv (sy tempW) time $=
  (sy coilHTC `mul` sy coilSA $/
  (sy wMass `mul` sy htCapW)) `mul`  (sy tempC $- sy tempW) `add`
  ((sy pcmHTC `mul` sy pcmSA $/
  (sy coilHTC `mul` sy coilSA)) `mul` (sy coilHTC `mul` sy coilSA $/
  (sy wMass `mul` sy htCapW)) `mul` (sy tempPCM $- sy tempW))

eBalanceOnWtrDerivEqn6 = deriv (sy tempW) time $=
  recip_ (sy tauW) `mul` (sy tempC $- sy tempW) `add` ((sy eta $/ sy tauW) `mul` (sy tempPCM $- sy tempW))

eBalanceOnWtrDerivEqn7 = deriv (sy tempW) time $=
  recip_ (sy tauW) `mul` ((sy tempC $- sy tempW) `add` (sy eta `mul` (sy tempPCM $- sy tempW)))

eBalanceOnWtrDerivEqnsIM1 :: [ModelExpr]
eBalanceOnWtrDerivEqnsIM1 = [eBalanceOnWtrDerivEqn1, eBalanceOnWtrDerivEqn2,
 eBalanceOnWtrDerivEqn3, eBalanceOnWtrDerivEqn4, eBalanceOnWtrDerivEqn5,
 eBalanceOnWtrDerivEqn6, eBalanceOnWtrDerivEqn7]

--
eBalanceOnPCMEqn1, eBalanceOnPCMEqn2, eBalanceOnPCMEqn3, eBalanceOnPCMEqn4 :: ModelExpr
eBalanceOnPCMEqn1 = sy pcmMass `mul` sy htCapSP `mul` deriv (sy tempPCM) time $=
  sy htFluxP `mul` sy pcmSA
eBalanceOnPCMEqn2 = sy pcmMass `mul` sy htCapSP `mul` deriv (sy tempPCM) time $=
  sy pcmHTC `mul` sy pcmSA `mul` (sy tempW $- sy tempPCM)
eBalanceOnPCMEqn3 = deriv (sy tempPCM) time $=
  ((sy pcmHTC `mul` sy pcmSA) $/ (sy pcmMass `mul` sy htCapSP)) `mul` (sy tempW $- sy tempPCM)
eBalanceOnPCMEqn4 = deriv (sy tempPCM) time $=
  recip_ (sy tauSP) `mul` (sy tempW $- sy tempPCM)

eBalanceOnPCMDerivEqnsIM2 :: [ModelExpr]
eBalanceOnPCMDerivEqnsIM2 = [eBalanceOnPCMEqn1, eBalanceOnPCMEqn2, eBalanceOnPCMEqn3, eBalanceOnPCMEqn4]

