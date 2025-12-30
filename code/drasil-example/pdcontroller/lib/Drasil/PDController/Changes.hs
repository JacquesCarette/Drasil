module Drasil.PDController.Changes where

import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Development as D
import Drasil.Sentence.Combinators (fromSources)

import Data.Drasil.Concepts.Documentation (likeChgDom)
import Data.Drasil.Concepts.PhysicalProperties (mass)

import Drasil.PDController.Assumptions
import Drasil.PDController.Concepts

likelyChgs :: [ConceptInstance]
likelyChgs = [likeChgPP]

likeChgPP :: ConceptInstance
likeChgPP = cic "likeChgPP" likeChgPPDesc "DC Gain and Time Constant" likeChgDom

likeChgPPDesc :: Sentence
likeChgPPDesc
  = foldlSent
      [D.toSent (atStartNP (the mass)) `sC` D.toSent (phraseNP (ccDampingCoeff `andThe` ccStiffCoeff)),
       S "may be changed to be supplied by the user",
       fromSources [aMass, aDampingCoeff, aStiffnessCoeff]]
