module Drasil.SWHS.Derivations where

import Data.Drasil.Quantities.Physics (time)

import Drasil.SWHS.Unitals (wMass, tauW, tauSP, pcmSA, pcmMass,
  pcmHTC, htCapW, htCapSP, eta, coilSA, coilHTC, htFluxC,
  htFluxP, tempC, tempW, tempPCM)

import Language.Drasil (ModelExprC(deriv), ExprC(..), ModelExpr, recip_)

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
  (sy wMass `mulRe` sy htCapW)) `mulRe`  (sy tempC $- sy tempW) `add`
  ((sy coilHTC `mulRe` sy coilSA $/
  (sy coilHTC `mulRe` sy coilSA)) `mulRe` (sy pcmHTC `mulRe` sy pcmSA $/
  (sy wMass `mulRe` sy htCapW)) `mulRe` (sy tempPCM $- sy tempW))

eBalanceOnWtrDerivEqn5 = deriv (sy tempW) time $=
  (sy coilHTC `mulRe` sy coilSA $/
  (sy wMass `mulRe` sy htCapW)) `mulRe`  (sy tempC $- sy tempW) `add`
  ((sy pcmHTC `mulRe` sy pcmSA $/
  (sy coilHTC `mulRe` sy coilSA)) `mulRe` (sy coilHTC `mulRe` sy coilSA $/
  (sy wMass `mulRe` sy htCapW)) `mulRe` (sy tempPCM $- sy tempW))

eBalanceOnWtrDerivEqn6 = deriv (sy tempW) time $=
  recip_ (sy tauW) `mulRe` (sy tempC $- sy tempW) `add` ((sy eta $/ sy tauW) `mulRe` (sy tempPCM $- sy tempW))

eBalanceOnWtrDerivEqn7 = deriv (sy tempW) time $=
  recip_ (sy tauW) `mulRe` ((sy tempC $- sy tempW) `add` (sy eta `mulRe` (sy tempPCM $- sy tempW)))

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
  ((sy pcmHTC `mulRe` sy pcmSA) $/ (sy pcmMass `mulRe` sy htCapSP)) `mulRe` (sy tempW $- sy tempPCM)
eBalanceOnPCMEqn4 = deriv (sy tempPCM) time $=
  recip_ (sy tauSP) `mulRe` (sy tempW $- sy tempPCM)

eBalanceOnPCMDerivEqnsIM2 :: [ModelExpr]
eBalanceOnPCMDerivEqnsIM2 = [eBalanceOnPCMEqn1, eBalanceOnPCMEqn2, eBalanceOnPCMEqn3, eBalanceOnPCMEqn4]

