{-# LANGUAGE PostfixOperators#-}
module Drasil.DblPend.Goals (goals, goalsInputs) where

import Control.Lens ((^.))

import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Development as D
import qualified Language.Drasil.Sentence.Combinators as S
import Data.Drasil.Concepts.Documentation (goalStmtDom)
import Data.Drasil.Concepts.Physics (gravitationalConst, motion)
import Data.Drasil.Concepts.Math (iAngle)
import qualified Data.Drasil.Quantities.PhysicalProperties as QPP (mass, len)
import Drasil.DblPend.Concepts (rod)

goals :: [ConceptInstance]
goals = [motionMass]

goalsInputs :: [Sentence]
goalsInputs = [D.toSent (phraseNP (theGen (\x -> pluralNP (x ^. term)) QPP.mass)) `sC`
               D.toSent (pluralNP (QPP.len `ofThe` rod)) `sC`
               D.toSent (pluralNP (iAngle `ofThe` QPP.mass)) `S.and_`
               D.toSent (phraseNP (the gravitationalConst))]

motionMass :: ConceptInstance
motionMass = cic "motionMass"
  (S "Calculate" +:+ D.toSent (pluralNP (motion `the_ofThe` QPP.mass)) !.) "motionMass" goalStmtDom
