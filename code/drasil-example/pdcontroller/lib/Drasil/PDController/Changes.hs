module Drasil.PDController.Changes where

import Data.Drasil.Concepts.Documentation (likeChgDom)
import Data.Drasil.Concepts.PhysicalProperties (mass)

import Drasil.PDController.Assumptions
import Drasil.PDController.Concepts
import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators

likelyChgs :: [ConceptInstance]
likelyChgs = [likeChgPP]

likeChgPP :: ConceptInstance
likeChgPP = cic "likeChgPP" likeChgPPDesc "DC Gain and Time Constant" likeChgDom

likeChgPPDesc :: Sentence
likeChgPPDesc
  = foldlSent
      [atStartNP (the mass) `sC` phraseNP (ccDampingCoeff `andThe` ccStiffCoeff),
       S "may be changed to be supplied by the user", 
       fromSources [aMass, aDampingCoeff, aStiffnessCoeff]]