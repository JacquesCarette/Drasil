module Drasil.GamePhysics.Derivations where

import Language.Drasil (eqSymb)
import Language.Drasil.ModelExpr

import Drasil.GamePhysics.Assumptions (assumpOT, assumpOD, assumpAD, assumpCT, assumpDI)
import Drasil.GamePhysics.References (chaslesWiki)
import Drasil.GamePhysics.Unitals (initRelVel, massj, mTot, normalVect,
  posCM, posj, velB, velO, rOB, finRelVel,
  velAP, velBP, rRot, velo_1, velo_2, timeT, time_1, time_2, massj,
   mTot, normalVect, velo_1, velo_2)

import Data.Drasil.Concepts.Math (rightHand)
import Data.Drasil.Concepts.Physics (rigidBody, twoD)

import qualified Data.Drasil.Quantities.Math as QM (orientation)
import qualified Data.Drasil.Quantities.Physics as QP (angularAccel, angularDisplacement, angularVelocity,
  displacement, linearAccel, linearDisplacement, linearVelocity, position,
  restitutionCoef, time, velocity,force, torque, kEnergy, energy, impulseV, chgInVelocity,
  acceleration, potEnergy, height, gravitationalAccel, momentOfInertia)

import qualified Data.Drasil.Quantities.PhysicalProperties as QPP (mass)


impulseVDerivEqn1 :: ModelExpr
impulseVDerivEqn1 =  sy QP.force $= sy QPP.mass `mulRe` sy QP.acceleration
                     $= sy QPP.mass `mulRe` deriv (sy QP.velocity) QP.time

impulseVDerivEqn2 :: ModelExpr -- TODO: Why does defint take a symbol as an argument? Shouldn't it be a UID?
impulseVDerivEqn2 = defint (eqSymb timeT) (sy time_1) (sy time_2) (sy QP.force) $=
                    sy QPP.mass `mulRe` defint (eqSymb QP.velocity) (sy velo_1) (sy velo_2) (exactDbl 1)

impulseVDerivEqn3 :: ModelExpr
impulseVDerivEqn3 = defint (eqSymb timeT) (sy time_1) (sy time_2) (sy QP.force)
                    $= (sy QPP.mass `mulRe` sy velo_2) $- (sy QPP.mass `mulRe` sy velo_1) 
                    $= sy QPP.mass `mulRe` sy QP.chgInVelocity
                                      
impulseVDerivEqns :: [ModelExpr]
impulseVDerivEqns = [impulseVDerivEqn1, impulseVDerivEqn2, impulseVDerivEqn3]

