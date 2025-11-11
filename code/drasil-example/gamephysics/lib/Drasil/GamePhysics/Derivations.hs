module Drasil.GamePhysics.Derivations where

import Language.Drasil (eqSymb, ModelExprC(..), ExprC(..), ModelExpr, LiteralC(..))

import Drasil.GamePhysics.Unitals (timeT, time_1, time_2, velo_1, velo_2)

import qualified Data.Drasil.Quantities.Physics as QP (force, time, velocity,
  acceleration, chgInVelocity)

import qualified Data.Drasil.Quantities.PhysicalProperties as QPP (mass)


impulseVDerivEqn1 :: ModelExpr
impulseVDerivEqn1 = sy QP.force $= sy QPP.mass $* sy QP.acceleration
                     $= sy QPP.mass $* deriv (sy QP.velocity) QP.time

impulseVDerivEqn2 :: ModelExpr -- TODO: Why does defint take a symbol as an argument? Shouldn't it be a UID?
impulseVDerivEqn2 = defint (eqSymb timeT) (sy time_1) (sy time_2) (sy QP.force) $=
                    sy QPP.mass $* defint (eqSymb QP.velocity) (sy velo_1) (sy velo_2) (exactDbl 1)

impulseVDerivEqn3 :: ModelExpr
impulseVDerivEqn3 = defint (eqSymb timeT) (sy time_1) (sy time_2) (sy QP.force)
                    $= (sy QPP.mass $* sy velo_2) $- (sy QPP.mass $* sy velo_1)
                    $= sy QPP.mass $* sy QP.chgInVelocity

impulseVDerivEqns :: [ModelExpr]
impulseVDerivEqns = [impulseVDerivEqn1, impulseVDerivEqn2, impulseVDerivEqn3]

