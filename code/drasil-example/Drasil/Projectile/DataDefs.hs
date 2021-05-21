module Drasil.Projectile.DataDefs (dataDefs, speedIX, speedIY) where

import Prelude hiding (sin, cos)
import Language.Drasil
import Utils.Drasil
import qualified Utils.Drasil.Sentence as S

import Theory.Drasil (DataDefinition, ddNoRefs)

import Data.Drasil.Quantities.Physics (speed, iSpeed, ixVel, iyVel, velocity)

import Drasil.Projectile.Figures (figLaunch)
import Drasil.Projectile.Unitals (launAngle)

dataDefs :: [DataDefinition]
dataDefs = [vecMag, speedIX, speedIY]

----------
vecMag, speedIX, speedIY :: DataDefinition
vecMag  = ddNoRefs vecMagQD  Nothing "vecMag"  [magNote]
speedIX = ddNoRefs speedIXQD Nothing "speedIX" [speedRef, figRef]
speedIY = ddNoRefs speedIYQD Nothing "speedIY" [speedRef, figRef]

vecMagQD, speedIXQD, speedIYQD :: QDefinition
vecMagQD  = mkQuantDef speed speedE
speedIXQD = mkQuantDef ixVel $ sy iSpeed `mulRe` cos (sy launAngle)
speedIYQD = mkQuantDef iyVel $ sy iSpeed `mulRe` sin (sy launAngle)

speedE :: Expr
speedE = norm $ sy velocity
----------
magNote :: Sentence
magNote = foldlSent [S "For a given", phrase velocity, S "vector", ch velocity `sC`
  S "the magnitude of the vector", sParen (E speedE) `S.isThe`
  S "scalar called", phrase speed]

speedRef :: Sentence
speedRef = ch iSpeed `S.sIs` S "from" +:+. makeRef2S vecMag

figRef :: Sentence
figRef = ch launAngle `S.sIs` S "shown in" +:+. makeRef2S figLaunch
