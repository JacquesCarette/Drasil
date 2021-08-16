{-# LANGUAGE PostfixOperators#-}
module Drasil.DblPendulum.Goals (goals, goalsInputs) where

import Language.Drasil
import Utils.Drasil.Concepts
import qualified Utils.Drasil.Sentence as S
import Data.Drasil.Concepts.Documentation (goalStmtDom)
import qualified Data.Drasil.Concepts.PhysicalProperties as CPP (len)
import Data.Drasil.Concepts.Physics (gravitationalConst, motion)
import Data.Drasil.Concepts.Math (iAngle)
import Drasil.DblPendulum.Concepts (massOrMasses, rodOrRods)


goals :: [ConceptInstance]
goals = [motionMass]

goalsInputs :: [Sentence]
goalsInputs = [phraseNP (the massOrMasses) `sC` 
               phraseNP (CPP.len `ofThe` rodOrRods) `sC` 
               phraseNP (iAngle `ofThe` massOrMasses) `S.and_` 
               phraseNP (the gravitationalConst)]

motionMass :: ConceptInstance
motionMass = cic "motionMass" 
  (S "Calculate" +:+ phraseNP (motion `the_ofThe` massOrMasses) !.) "motionMass" goalStmtDom
