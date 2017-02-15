module Drasil.GamePhysics.Changes where

import Language.Drasil
import Drasil.GamePhysics.Modules

lcs :: [LCChunk]
lcs = [lc1, lc2, lc3, lc4, lc5, lc6, lc7, lc8, lc9, lc10, lc11, lc12, lc13,
  lc14]

ucs :: [UCChunk]
ucs = [uc1, uc2, uc3, uc4, uc5, uc6, uc7]

-- Likely Changes --

lc1, lc2, lc3, lc4, lc5, lc6, lc7, lc8, lc9, lc10, lc11, lc12, lc13,
  lc14 :: LCChunk

lc1 = LCChunk (nc "hardware"
  "The specific hardware on which the software is running.")
  [mod_hw]

lc2 = LCChunk (nc "rigid body"
  ("The data structure of the physical properties of an object such as " ++ 
  "the object's mass, position and velocity."))
  [mod_body]

lc3 = LCChunk (nc "shape"
  ("The data structure of the surface properties of an object such as " ++
  "the object's friction and elasticity."))
  [mod_shape, mod_circle, mod_segment, mod_poly]

lc4 = LCChunk (nc "space"
  "How all the rigid bodies and shapes interact together.")
  [mod_space]

lc5 = LCChunk (nc "arbiter"
  ("The data structure containing collision information such as the " ++
  "objects that collide and their masses."))
  [mod_arbiter]

lc6 = LCChunk (nc "control"
  ("How the overall control of the simulation is orchestrated, " ++
  "including the input and output of data."))
  [mod_control]

lc7 = LCChunk (nc "vector"
  "The implementation of mathematical vectors.")
  [mod_vector]

lc8 = LCChunk (nc "bb"
  "The implementation of bounding box structures")
  [mod_bb]

lc9 = LCChunk (nc "transform"
  "The implementation of affine transformation matrices.")
  [mod_vector]

lc10 = LCChunk (nc "spatial index"
  "How the simulation space is spatially indexed.")
  [mod_spatial]

lc11 = LCChunk (nc "collision"
  "The algorithms used for solving collisions.")
  [mod_coll]

lc12 = LCChunk (nc "array"
  "The implementation of the sequence (array) data structure.")
  [mod_seq]

lc13 = LCChunk (nc "tree"
  "The implementation of the linked (tree) data structure.")
  [mod_linked]

lc14 = LCChunk (nc "hash table"
  "The implementation of the associative (hash table) data structure.")
  [mod_assoc]

-- Unlikely Changes --

uc1, uc2, uc3, uc4, uc5, uc6, uc7 :: UCChunk

uc1 = nc "IO"
  ("Input/Output devices (Input: File and/or Keyboard, Output: File, " ++
  "Memory, and/or Screen).")

uc2 = nc "inputsource"
  "There will always be a source of input data external to the software."

uc3 = nc "output" "Output data are displayed to the output device."

uc4 = nc "goal"
  ("The goal of the system is to simulate the interactions of 2D rigid " ++
  "bodies.")

uc5 = nc "Cartesian" "A Cartesian coordinate system is used."

uc6 = nc "rigid" "All objects are rigid bodies."

uc7 = nc "2D" "All objects are 2D."
