module Data.Drasil.Concepts.PhysicalProperties where

import Language.Drasil

gaseous, liquid, solid :: ConceptChunk

gaseous = makeCC "Gas" "gaseous state"
liquid = makeCC "Liquid" "liquid state"
solid = makeCC "Solid" "solid state"
