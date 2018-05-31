module Data.Drasil.Concepts.PhysicalProperties where

import Language.Drasil
import Data.Drasil.Concepts.Documentation

gaseous, liquid, solid, ctrOfMass, density, mass, len, dimension,
  vol, flexure :: ConceptChunk

gaseous    = dcc "gaseous"    (cn''' "gas"        ) "gaseous state"
liquid     = dcc "liquid"     (cn' "liquid"       ) "liquid state"
solid      = dcc "solid"      (cn' "solid"        ) "solid state"
ctrOfMass  = dcc "ctrOfMass"  (cn "centre of mass") --FIXME: Plural?
  "The mean location of the distribution of mass of the object."
dimension  = dcc "dimension"  (cn' "dimension"    ) 
  "any of a set of basic kinds of quantity, as mass, length, and time"
density    = dcc "density"    (cnIES "density"    ) "mass per unit volume"
flexure    = dcc "flexure"    (cn' "flexure"      ) "a bent or curved part"
len        = dcc "length"     (cn' "length"       )
  ("the straight-line distance between two points along an object. " ++
  "typically used to represent the size of an object from one end to the other.")
mass       = dcc "mass"       (cn''' "mass"       )
  "the quantity of matter in a body"
vol        = dcc "volume"     (cn' "volume"       )
  "the amount of space that a substance or object occupies."


materialProprty :: NamedChunk
materialProprty = compoundNC material_ property
