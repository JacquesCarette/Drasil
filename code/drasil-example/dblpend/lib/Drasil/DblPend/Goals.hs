{-# LANGUAGE PostfixOperators#-}
module Drasil.DblPend.Goals (goals, goalsInputs) where

import Control.Lens ((^.))

import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Development as D
import qualified Language.Drasil.Sentence.Combinators as S
import Data.Drasil.Concepts.Documentation (goalStmtDom)
import qualified Data.Drasil.Concepts.PhysicalProperties as CPP (mass, len)
import Data.Drasil.Concepts.Physics (gravitationalConst, motion)
import Data.Drasil.Concepts.Math (iAngle)
import Drasil.DblPend.Concepts (rod)

goals :: [ConceptInstance]
goals = [motionMass]

goalsInputs :: [Sentence]
goalsInputs = [D.toSent (phraseNP (theGen (\x -> pluralNP (x ^. term)) CPP.mass)) `sC`
               D.toSent (pluralNP (CPP.len `ofThe` rod)) `sC`
               D.toSent (pluralNP (iAngle `ofThe` CPP.mass)) `S.and_`
               D.toSent (phraseNP (the gravitationalConst))]

motionMass :: ConceptInstance
motionMass = cic "motionMass"
  (S "Calculate" +:+ D.toSent (pluralNP (motion `the_ofThe` CPP.mass)) !.) "motionMass" goalStmtDom
