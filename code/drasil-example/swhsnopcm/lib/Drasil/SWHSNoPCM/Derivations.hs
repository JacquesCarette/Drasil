module Drasil.SWHSNoPCM.Derivations (
  eBalanceOnWtrDerivEqns
) where

import Language.Drasil (ExprC(..), ModelExprC(..), ModelExpr, recip_)

import Data.Drasil.Quantities.Physics (time)
import Drasil.SWHS.Unitals (coilHTC, coilSA, htCapW, htFluxC, tauW, tempC,
  tempW, wMass)

----------------------------------------------
--    Derivation of eBalanceOnWtr           --
----------------------------------------------
eBalanceOnWtrDerivEqn1, eBalanceOnWtrDerivEqn2, eBalanceOnWtrDerivEqn3, eBalanceOnWtrDerivEqn4 :: ModelExpr

eBalanceOnWtrDerivEqn1 = sy wMass $* sy htCapW $* deriv (sy tempW) time $=
  sy htFluxC $* sy coilSA

eBalanceOnWtrDerivEqn2 = sy wMass $* sy htCapW $* deriv (sy tempW) time $=
  sy coilHTC $* sy coilSA $* (sy tempC $- sy tempW)

eBalanceOnWtrDerivEqn3 = deriv (sy tempW) time $=
  (sy coilHTC $* sy coilSA $/
  (sy wMass $* sy htCapW)) $* (sy tempC $- sy tempW)

eBalanceOnWtrDerivEqn4 =
  deriv (sy tempW) time $= recip_ (sy tauW) $* (sy tempC $- sy tempW)

eBalanceOnWtrDerivEqns :: [ModelExpr]
eBalanceOnWtrDerivEqns = [eBalanceOnWtrDerivEqn1, eBalanceOnWtrDerivEqn2, eBalanceOnWtrDerivEqn3, eBalanceOnWtrDerivEqn4]
