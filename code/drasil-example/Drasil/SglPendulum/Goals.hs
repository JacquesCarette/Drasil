{-# LANGUAGE PostfixOperators#-}
module Drasil.SglPendulum.Goals (goals, goalsInputs) where

import Language.Drasil
import Utils.Drasil.Concepts
import qualified Utils.Drasil.Sentence as S
import qualified Utils.Drasil.NounPhrase as NP

import Data.Drasil.Concepts.Documentation (goalStmtDom)
import qualified Data.Drasil.Concepts.PhysicalProperties as CPP (mass, len)
import Data.Drasil.Concepts.Physics (gravitationalConst, motion)
import Data.Drasil.Concepts.Math (iAngle)
import Drasil.DblPendulum.Concepts (rod)


goals :: [ConceptInstance]
goals = [motionMass]


goalsInputs :: [Sentence]
goalsInputs = [phraseNP (the CPP.mass `NP.and_` (CPP.len  `ofThe` rod)) `sC` 
        phraseNP (iAngle `ofThe` CPP.mass) `S.and_` phraseNP (the gravitationalConst) ]

motionMass :: ConceptInstance
motionMass = cic "motionMass" 
  (S "Calculate" +:+ phraseNP (motion `the_ofThe` CPP.mass) !.)
  "Motion-of-the-mass" goalStmtDom
