module Drasil.Projectile.Changes (likelyChgs) where

import Language.Drasil

import Data.Drasil.Concepts.Documentation (likeChgDom)
import Drasil.Sentence.Combinators (chgsStart)

import Drasil.Projectile.Assumptions (neglectDrag)
import Drasil.Projectile.Concepts (projectile)

{--LIKELY CHANGES--}

likelyChgs :: [ConceptInstance]
likelyChgs = [considerAirDrag]

considerAirDrag :: ConceptInstance
considerAirDrag = cic "considerAirDrag" considerAirDragDesc "Consider-Air-Drag" likeChgDom

considerAirDragDesc :: Sentence
considerAirDragDesc = foldlSent [chgsStart neglectDrag (S "The software may be changed to"),
  S "consider the effect of air drag on the", phrase projectile, S "during its flight"]
