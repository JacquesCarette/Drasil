module Data.Drasil.Concepts.PhysicalProperties where

import Language.Drasil

gaseous, liquid, solid, ctrOfMass, density, mass, length :: ConceptChunk

gaseous = dcc "gaseous" (cn''' "Gas") "gaseous state"
liquid  = dcc "liquid" (cn' "Liquid") "liquid state"
solid   = dcc "solid" (cn' "Solid") "solid state"

ctrOfMass = dcc "ctrOfMass" (cn "centre of mass") --FIXME: Plural?
  "The mean location of the distribution of mass of the object."

density = dcc "density" (cnIES "density") ""
length = dcc "length" (cn' "length")
  ("the straight-line distance between two points along an object. " ++
  "Typically used to represent the size of an object from one end to the other.")
mass = dcc "mass" (cn''' "mass") "the quantity of matter in a body"

  
--TODO: Add density