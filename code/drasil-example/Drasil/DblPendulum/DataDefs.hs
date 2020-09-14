module Drasil.DblPendulum.DataDefs (dataDefs, positionIX) where

import Prelude hiding (sin)
import Language.Drasil
import Utils.Drasil

import Theory.Drasil (DataDefinition, ddNoRefs, mkQuantDef)
import Drasil.DblPendulum.Figures (figMotion)
import qualified Data.Drasil.Quantities.Physics as QP (ixPos)

--import Drasil.DblPendulum.Unitals (lenRod)

dataDefs :: [DataDefinition]
dataDefs = [positionIX]

----------
{-vecMag :: DataDefinition
vecMag = ddNoRefs vecMagQD Nothing "vecMag" [magNote]

vecMagQD :: QDefinition
vecMagQD = mkQuantDef speed vecMagEqn

vecMagEqn :: Expr
vecMagEqn = UnaryOp Abs (sy velocity)-}

----------
positionIX :: DataDefinition
positionIX = ddNoRefs positionIXQD Nothing "positionIX" [positionRef, figRef]

positionIXQD :: QDefinition
positionIXQD = mkQuantDef QP.ixPos positionIXEqn

positionIXEqn :: Expr
positionIXEqn = sy QP.ixPos * sin (sy QP.ixPos)

----------
-- speedIY :: DataDefinition
-- speedIY = ddNoRefs speedIYQD Nothing "speedIY" [speedRef, figRef]

-- speedIYQD :: QDefinition
-- speedIYQD = mkQuantDef iyVel speedIYEqn

-- speedIYEqn :: Expr
-- speedIYEqn = sy iSpeed * sin (sy launAngle)

-- ----------
-- magNote :: Sentence
-- magNote = foldlSent [S "For a given", phrase velocity, S "vector", ch velocity `sC`
--   S "the magnitude of the vector", sParen (E $ UnaryOp Abs (sy velocity)) `isThe`
--   S "scalar called", phrase speed]

-- speedRef :: Sentence
-- speedRef = ch iSpeed `sIs` S "from" +:+. makeRef2S vecMag

figRef :: Sentence
figRef = ch QP.ixPos `sIs` S "shown in" +:+. makeRef2S figMotion

positionRef :: Sentence
positionRef = ch QP.ixPos `sIs` S "from freezy"
