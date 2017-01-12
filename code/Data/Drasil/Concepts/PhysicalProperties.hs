module Data.Drasil.Concepts.PhysicalProperties where

import Language.Drasil

gaseous, liquid, solid, ctrOfMass, mass, length :: ConceptChunk

gaseous = dcc "gaseous" "Gas" "gaseous state"
liquid  = dcc "liquid" "Liquid" "liquid state"
solid   = dcc "solid" "Solid" "solid state"

ctrOfMass = dcc "ctrOfMass" "centre of mass" 
  "The mean location of the distribution of mass of the object."

mass = dcc "mass" "mass" "the quantity of matter in a body"
length = dcc "length" "length" 
  ("the straight-line distance between two points along an object. " ++
  "Typically used to represent the size of an object from one end to the other.")