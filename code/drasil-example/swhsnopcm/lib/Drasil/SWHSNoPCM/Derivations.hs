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

eBalanceOnWtrDerivEqn1 = sy wMass `mul` sy htCapW `mul` deriv (sy tempW) time $=
  sy htFluxC `mul` sy coilSA

eBalanceOnWtrDerivEqn2 = sy wMass `mul` sy htCapW `mul` deriv (sy tempW) time $=
  sy coilHTC `mul` sy coilSA `mul`  (sy tempC $- sy tempW)

eBalanceOnWtrDerivEqn3 = deriv (sy tempW) time $=
  (sy coilHTC `mul` sy coilSA $/
  (sy wMass `mul` sy htCapW)) `mul`  (sy tempC $- sy tempW)

eBalanceOnWtrDerivEqn4 =
  deriv (sy tempW) time $= recip_ (sy tauW) `mul` (sy tempC $- sy tempW)

eBalanceOnWtrDerivEqns :: [ModelExpr]
eBalanceOnWtrDerivEqns = [eBalanceOnWtrDerivEqn1, eBalanceOnWtrDerivEqn2, eBalanceOnWtrDerivEqn3, eBalanceOnWtrDerivEqn4]
