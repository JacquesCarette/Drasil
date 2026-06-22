-- | Defines concepts used to describe physical properties.
module Data.Drasil.Concepts.PhysicalProperties where

import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators
import Drasil.Database (mkUid)

import Data.Drasil.Concepts.Documentation (material_, property)
import Data.Drasil.Concepts.Math (centre)

-- | Collects all physical property-related concepts.
physicalcon :: [ConceptChunk]
physicalcon = [gaseous, liquid, solid, ctrOfMass, dimension, flexure]

-- * Physical Properties

gaseous, liquid, solid, ctrOfMass, density, specWeight, mass, len, dimension,
  vol, flexure :: ConceptChunk

gaseous    = cncpt''' (mkUid "gaseous")    (cn''' "gas"          ) (S "gaseous state")
liquid     = cncpt''' (mkUid "liquid")     (cn' "liquid"         ) (S "liquid state")
solid      = cncpt''' (mkUid "solid")      (cn' "solid"          ) (S "solid state")
ctrOfMass  = cncpt''' (mkUid "ctrOfMass")  (centre `of_PS` mass  ) (S "the mean location of the distribution of mass of the object")
dimension  = cncpt''' (mkUid "dimension")  (cn' "dimension"      ) (S "any of a set of basic kinds of quantity, as mass, length, and time")
density    = cncpt''' (mkUid "density")    (cnIES "density"      ) (S "the mass per unit volume")
specWeight = cncpt''' (mkUid "specWeight") (cn' "specific weight") (S "the weight per unit volume")
flexure    = cncpt''' (mkUid "flexure")    (cn' "flexure"        ) (S "a bent or curved part")
len        = cncpt''' (mkUid "length")     (cn' "length"         ) (S ("the straight-line distance between two points along an object, " ++
                                                       "typically used to represent the size of an object from one end to the other"))
mass       = cncpt''' (mkUid "mass")       (cn''' "mass"         ) (S "the quantity of matter in a body")
vol        = cncpt''' (mkUid "volume")     (cn' "volume"         ) (S "the amount of space that a substance or object occupies")

materialProprty :: IdeaDict
materialProprty = compoundNC material_ property
