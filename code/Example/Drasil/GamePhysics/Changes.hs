module Drasil.GamePhysics.Changes where

import Language.Drasil
import Drasil.GamePhysics.Modules
import Data.Drasil.Modules
import Drasil.GamePhysics.Concepts (chipmunk)

lcs :: [LCChunk]
lcs = [lc1, lc2, lc3, lc4, lc5, lc6, lc7, lc8, lc9, lc10, lc11, lc12, lc13,
  lc14]

ucs :: [UCChunk]
ucs = [uc1, uc2, uc3, uc4, uc5, uc6, uc7]

-- Likely Changes --

lc1, lc2, lc3, lc4, lc5, lc6, lc7, lc8, lc9, lc10, lc11, lc12, lc13,
  lc14 :: LCChunk

lc1 = LCChunk (nw $ npnc "hardware"
  (nounPhraseSP "The specific hardware on which the software is running."))
  [mod_hw]

lc2 = LCChunk (nw $ npnc "rigid body"
  (nounPhraseSP $ 
  "The data structure of the physical properties of an object such as " ++ 
  "the object's mass, position and velocity."))
  [mod_body]

lc3 = LCChunk (nw $ npnc "shape"
  (nounPhraseSP $ 
  "The data structure of the surface properties of an object such as " ++
  "the object's friction and elasticity."))
  [mod_shape, mod_circle, mod_segment, mod_poly]

lc4 = LCChunk (nw $ npnc "space"
  (nounPhraseSP $ "How all the rigid bodies and shapes interact together."))
  [mod_space]

lc5 = LCChunk (nw $ npnc "arbiter" (nounPhraseSP $ 
  "The data structure containing collision information such as the " ++
  "objects that collide and their masses."))
  [mod_arbiter]

lc6 = LCChunk (nw $ npnc "control" (nounPhraseSP $ 
  "How the overall control of the simulation is orchestrated, " ++
  "including the input and output of data."))
  [mod_control]

lc7 = LCChunk (nw $ npnc "vector" (nounPhraseSP $ 
  "The implementation of mathematical vectors."))
  [(mod_vector chipmunk)]

lc8 = LCChunk (nw $ npnc "bb" (nounPhraseSP $ 
  "The implementation of bounding box structures"))
  [mod_bb]

lc9 = LCChunk (nw $ npnc "transform" (nounPhraseSP $ 
  "The implementation of affine transformation matrices."))
  [(mod_vector chipmunk)]

lc10 = LCChunk (nw $ npnc "spatial index" (nounPhraseSP $ 
  "How the simulation space is spatially indexed."))
  [mod_spatial]

lc11 = LCChunk (nw $ npnc "collision" (nounPhraseSP $ 
  "The algorithms used for solving collisions."))
  [mod_coll]

lc12 = LCChunk (nw $ npnc "array" (nounPhraseSP $ 
  "The implementation of the sequence (array) data structure."))
  [(mod_seq chipmunk)]

lc13 = LCChunk (nw $ npnc "tree" (nounPhraseSP $ 
  "The implementation of the linked (tree) data structure."))
  [(mod_linked chipmunk)]

lc14 = LCChunk (nw $ npnc "hash table" (nounPhraseSP $ 
  "The implementation of the associative (hash table) data structure."))
  [(mod_assoc chipmunk)]

-- Unlikely Changes --

uc1, uc2, uc3, uc4, uc5, uc6, uc7 :: UCChunk

uc1 = nw $ npnc "IO" (nounPhraseSP $ 
  "Input/Output devices (Input: File and/or Keyboard, Output: File, " ++
  "Memory, and/or Screen).")

uc2 = nw $ npnc "inputsource" (nounPhraseSP $ 
  "There will always be a source of input data external to the software.")

uc3 = nw $ npnc "output" (nounPhraseSP $ 
  "Output data are displayed to the output device.")

uc4 = nw $ npnc "goal" (nounPhraseSP $ 
  "The goal of the system is to simulate the interactions of 2D rigid bodies.")

uc5 = nw $ npnc "Cartesian" 
  (nounPhraseSP "A Cartesian coordinate system is used.")

uc6 = nw $ npnc "rigid" (nounPhraseSP "All objects are rigid bodies.")

uc7 = nw $ npnc "2D" (nounPhraseSP "All objects are 2D.")
