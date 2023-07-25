{-# LANGUAGE PostfixOperators#-}
 module Drasil.SglPend.Goals (goals, goalsInputs) where

 import Language.Drasil
 import Language.Drasil.Chunk.Concept.NamedCombinators
 import qualified Language.Drasil.Sentence.Combinators as S
 import qualified Language.Drasil.NounPhrase.Combinators as NP

 import Data.Drasil.Concepts.Documentation (goalStmtDom)
 import qualified Data.Drasil.Concepts.PhysicalProperties as CPP (mass, len)
 import Data.Drasil.Concepts.Physics (gravitationalConst, motion)
 import Data.Drasil.Concepts.Math (iAngle)
 import Drasil.DblPend.Concepts (rod)


 goals :: [ConceptInstance]
 goals = [motionMass]

 goalsInputs :: [Sentence]
 goalsInputs = [phraseNP (the CPP.mass `NP.and_` (CPP.len  `ofThe` rod)) `sC` 
         phraseNP (iAngle `ofThe` CPP.mass) `S.and_` phraseNP (the gravitationalConst) ]

 motionMass :: ConceptInstance
 motionMass = cic "motionMass" 
   (S "Calculate" +:+ phraseNP (motion `the_ofThe` CPP.mass) !.)
   "Motion-of-the-mass" goalStmtDom
