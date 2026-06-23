{-# LANGUAGE PostfixOperators#-}
module Drasil.SglPend.Goals (goals, goalsInputs) where

import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Development as D
import qualified Language.Drasil.Sentence.Combinators as S
import qualified Language.Drasil.NaturalLanguage.English.NounPhrase.Combinators as NP

import Data.Drasil.Concepts.Documentation (goalStmtDom)
import Data.Drasil.Concepts.Physics (gravitationalConst, motion)
import Data.Drasil.Concepts.Math (iAngle)
import qualified Data.Drasil.Quantities.PhysicalProperties as QPP (mass, len)
import Drasil.DblPend.Concepts (rod)

goals :: [ConceptInstance]
goals = [motionMass]

goalsInputs :: [Sentence]
goalsInputs = [D.toSent (phraseNP (the QPP.mass `NP.and_` (QPP.len `ofThe` rod))) `sC`
        D.toSent (phraseNP (iAngle `ofThe` QPP.mass)) `S.and_` D.toSent (phraseNP (the gravitationalConst)) ]

motionMass :: ConceptInstance
motionMass = cic "motionMass"
  (S "Calculate" +:+ D.toSent (phraseNP (motion `the_ofThe` QPP.mass)) !.)
  "Motion-of-the-mass" goalStmtDom
