module Drasil.SWHS.Derivations where

import Data.Drasil.Quantities.Physics (time)

import Drasil.SWHS.Unitals (wMass, tauW, tauSP, pcmSA, pcmMass,
  pcmHTC, htCapW, htCapSP, eta, coilSA, coilHTC, htFluxC,
  htFluxP, tempC, tempW, tempPCM)

import Language.Drasil (ModelExprC(deriv), ExprC(..), ModelExpr, recip_)

--
eBalanceOnWtrDerivEqn1, eBalanceOnWtrDerivEqn2, eBalanceOnWtrDerivEqn3,
 eBalanceOnWtrDerivEqn4, eBalanceOnWtrDerivEqn5, eBalanceOnWtrDerivEqn6, eBalanceOnWtrDerivEqn7 :: ModelExpr

eBalanceOnWtrDerivEqn1 = sy wMass $*  sy htCapW $*  deriv (sy tempW) time $=
  sy htFluxC $*  sy coilSA $- (sy htFluxP $*  sy pcmSA)

eBalanceOnWtrDerivEqn2 = sy wMass $*  sy htCapW $*  deriv (sy tempW) time $=
  sy coilHTC $*  sy coilSA $*  (sy tempC $- sy tempW) $-
  (sy pcmHTC $*  sy pcmSA $*  (sy tempW $- sy tempPCM))

eBalanceOnWtrDerivEqn3 = deriv (sy tempW) time $=
  (sy coilHTC $*  sy coilSA $/
  (sy wMass $*  sy htCapW)) $*  (sy tempC $- sy tempW) $-
  ((sy pcmHTC $*  sy pcmSA $/
  (sy wMass $*  sy htCapW)) $*  (sy tempW $- sy tempPCM))

eBalanceOnWtrDerivEqn4 = deriv (sy tempW) time $=
  (sy coilHTC $*  sy coilSA $/
  (sy wMass $*  sy htCapW)) $*   (sy tempC $- sy tempW) `add`
  ((sy coilHTC $*  sy coilSA $/
  (sy coilHTC $*  sy coilSA)) $*  (sy pcmHTC $*  sy pcmSA $/
  (sy wMass $*  sy htCapW)) $*  (sy tempPCM $- sy tempW))

eBalanceOnWtrDerivEqn5 = deriv (sy tempW) time $=
  (sy coilHTC $*  sy coilSA $/
  (sy wMass $*  sy htCapW)) $*   (sy tempC $- sy tempW) `add`
  ((sy pcmHTC $*  sy pcmSA $/
  (sy coilHTC $*  sy coilSA)) $*  (sy coilHTC $*  sy coilSA $/
  (sy wMass $*  sy htCapW)) $*  (sy tempPCM $- sy tempW))

eBalanceOnWtrDerivEqn6 = deriv (sy tempW) time $=
  recip_ (sy tauW) $*  (sy tempC $- sy tempW) `add` ((sy eta $/ sy tauW) $*  (sy tempPCM $- sy tempW))

eBalanceOnWtrDerivEqn7 = deriv (sy tempW) time $=
  recip_ (sy tauW) $*  ((sy tempC $- sy tempW) `add` (sy eta $*  (sy tempPCM $- sy tempW)))

eBalanceOnWtrDerivEqnsIM1 :: [ModelExpr]
eBalanceOnWtrDerivEqnsIM1 = [eBalanceOnWtrDerivEqn1, eBalanceOnWtrDerivEqn2,
 eBalanceOnWtrDerivEqn3, eBalanceOnWtrDerivEqn4, eBalanceOnWtrDerivEqn5,
 eBalanceOnWtrDerivEqn6, eBalanceOnWtrDerivEqn7]

--
eBalanceOnPCMEqn1, eBalanceOnPCMEqn2, eBalanceOnPCMEqn3, eBalanceOnPCMEqn4 :: ModelExpr
eBalanceOnPCMEqn1 = sy pcmMass $*  sy htCapSP $*  deriv (sy tempPCM) time $=
  sy htFluxP $*  sy pcmSA
eBalanceOnPCMEqn2 = sy pcmMass $*  sy htCapSP $*  deriv (sy tempPCM) time $=
  sy pcmHTC $*  sy pcmSA $*  (sy tempW $- sy tempPCM)
eBalanceOnPCMEqn3 = deriv (sy tempPCM) time $=
  ((sy pcmHTC $*  sy pcmSA) $/ (sy pcmMass $*  sy htCapSP)) $*  (sy tempW $- sy tempPCM)
eBalanceOnPCMEqn4 = deriv (sy tempPCM) time $=
  recip_ (sy tauSP) $*  (sy tempW $- sy tempPCM)

eBalanceOnPCMDerivEqnsIM2 :: [ModelExpr]
eBalanceOnPCMDerivEqnsIM2 = [eBalanceOnPCMEqn1, eBalanceOnPCMEqn2, eBalanceOnPCMEqn3, eBalanceOnPCMEqn4]

