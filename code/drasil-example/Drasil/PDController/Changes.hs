module Drasil.PDController.Changes where

import Data.Drasil.Concepts.Documentation (likeChgDom)
import Data.Drasil.Concepts.PhysicalProperties (mass)

import Drasil.PDController.Assumptions
import Drasil.PDController.Concepts
import Language.Drasil
import Utils.Drasil

likelyChgs :: [ConceptInstance]
likelyChgs = [likeChgPP]

likeChgPP :: ConceptInstance
likeChgPP = cic "likeChgPP" likeChgPPDesc "DC Gain and Time Constant" likeChgDom

likeChgPPDesc :: Sentence
likeChgPPDesc
  = foldlSent
      [S "The", phrase mass, S ", ", phrase ccDampingCoeff, S " and the",
       phrase ccStiffCoeff,
       S "may be changed to be supplied by the user (from ",
       foldlList Comma List [makeRef2S aMass, makeRef2S aDampingCoeff,
       makeRef2S aStiffnessCoeff] <> S ")"]
