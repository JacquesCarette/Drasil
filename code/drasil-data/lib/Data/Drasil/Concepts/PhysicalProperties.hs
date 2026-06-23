-- | Defines concepts used to describe physical properties.
module Data.Drasil.Concepts.PhysicalProperties where

import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators
import Drasil.Database (mkUid)

import Data.Drasil.Concepts.Documentation (material_, property)
import Data.Drasil.Concepts.Math (centre)
import Data.Drasil.Quantities.PhysicalProperties (mass)

-- | Collects all physical property-related concepts.
physicalcon :: [ConceptChunk]
physicalcon = [gaseous, liquid, solid, ctrOfMass, dimension, flexure]

-- * Physical Properties

gaseous, liquid, solid, ctrOfMass, dimension, flexure :: ConceptChunk

gaseous    = cncpt''' (mkUid "gaseous")    (cn''' "gas"          ) (S "gaseous state")
liquid     = cncpt''' (mkUid "liquid")     (cn' "liquid"         ) (S "liquid state")
solid      = cncpt''' (mkUid "solid")      (cn' "solid"          ) (S "solid state")
ctrOfMass  = cncpt''' (mkUid "ctrOfMass")  (centre `of_PS` mass  ) (S "the mean location of the distribution of mass of the object")
dimension  = cncpt''' (mkUid "dimension")  (cn' "dimension"      ) (S "any of a set of basic kinds of quantity, as mass, length, and time")
flexure    = cncpt''' (mkUid "flexure")    (cn' "flexure"        ) (S "a bent or curved part")

materialProprty :: IdeaDict
materialProprty = compoundNC material_ property
