{-# LANGUAGE PostfixOperators#-}
module Drasil.DblPendulum.Goals (goals, goalsInputs) where

import Language.Drasil
import Utils.Drasil.Concepts
import qualified Utils.Drasil.Sentence as S
import Data.Drasil.Concepts.Documentation (goalStmtDom)
import qualified Data.Drasil.Concepts.PhysicalProperties as CPP (mass, len)
import Data.Drasil.Concepts.Physics (gravitationalConst, motion)
import Data.Drasil.Concepts.Math (iAngle)
import Drasil.DblPendulum.Concepts (rod)


goals :: [ConceptInstance]
goals = [motionMass]


goalsInputs :: [Sentence]
goalsInputs = [phraseNP (theGen plural CPP.mass) `sC` 
               pluralNP (CPP.len `ofThe` rod) `sC` 
               pluralNP (iAngle `ofThe` CPP.mass) `S.and_` 
               phraseNP (the gravitationalConst)]

motionMass :: ConceptInstance
motionMass = cic "motionMass" 
  (S "Calculate" +:+ pluralNP (motion `the_ofThe` CPP.mass) !.)
  "motionMass" goalStmtDom
