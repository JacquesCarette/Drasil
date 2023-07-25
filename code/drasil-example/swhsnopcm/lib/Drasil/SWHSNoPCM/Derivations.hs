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

eBalanceOnWtrDerivEqn1 = sy wMass `mulRe` sy htCapW `mulRe` deriv (sy tempW) time $=
  sy htFluxC `mulRe` sy coilSA

eBalanceOnWtrDerivEqn2 = sy wMass `mulRe` sy htCapW `mulRe` deriv (sy tempW) time $=
  sy coilHTC `mulRe` sy coilSA `mulRe`  (sy tempC $- sy tempW)

eBalanceOnWtrDerivEqn3 = deriv (sy tempW) time $=
  (sy coilHTC `mulRe` sy coilSA $/
  (sy wMass `mulRe` sy htCapW)) `mulRe`  (sy tempC $- sy tempW)

eBalanceOnWtrDerivEqn4 =
  deriv (sy tempW) time $= recip_ (sy tauW) `mulRe` (sy tempC $- sy tempW)

eBalanceOnWtrDerivEqns :: [ModelExpr]
eBalanceOnWtrDerivEqns = [eBalanceOnWtrDerivEqn1, eBalanceOnWtrDerivEqn2, eBalanceOnWtrDerivEqn3, eBalanceOnWtrDerivEqn4]
