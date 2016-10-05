module Drasil.GamePhysics.Changes where

import Language.Drasil
import Drasil.GamePhysics.ChipmunkModules

lcs :: [LCChunk]
lcs = [lc1, lc2, lc3, lc4, lc5, lc6, lc7, lc8, lc9, lc10, lc11, lc12, lc13,
    lc14]

ucs :: [UCChunk]
ucs = [uc1, uc2, uc3, uc4, uc5, uc6, uc7]

-- Likely Changes --

lc1, lc2, lc3, lc4, lc5, lc6, lc7, lc8, lc9, lc10, lc11, lc12, lc13,
    lc14 :: LCChunk

lc1 = LCChunk (CC "hardware"
    (S "The specific hardware on which the software is running." ))
    [mod_hw]

lc2 = LCChunk (CC "rigid body"
    (S "The data structure of the physical properties of an object such as " :+:
    S "the object's mass, position and velocity."))
    [mod_body]

lc3 = LCChunk (CC "shape"
    (S "The data structure of the surface properties of an object such as " :+:
    S "the object's friction and elasticity."))
    [mod_shape, mod_circle, mod_segment, mod_poly]

lc4 = LCChunk (CC "space"
    (S "How all the rigid bodies and shapes interact together."))
    [mod_space]

lc5 = LCChunk (CC "arbiter"
    (S "The data structure containing collision information such as the " :+:
    S "objects that collide and their masses."))
    [mod_arbiter]

lc6 = LCChunk (CC "control"
    (S "How the overall control of the simulation is orchestrated, " :+:
    S "including the input and output of data."))
    [mod_control]

lc7 = LCChunk (CC "vector"
    (S "The implementation of mathematical vectors."))
    [mod_vector]

lc8 = LCChunk (CC "bb"
    (S "The implementation of bounding box structures"))
    [mod_bb]

lc9 = LCChunk (CC "transform"
    (S "The implementation of affine transformation matrices."))
    [mod_vector]

lc10 = LCChunk (CC "spatial index"
    (S "How the simulation space is spatially indexed."))
    [mod_spatial]

lc11 = LCChunk (CC "collision"
    (S "The algorithms used for solving collisions."))
    [mod_coll]

lc12 = LCChunk (CC "array"
    (S "The implementation of the sequence (array) data structure."))
    [mod_seq]

lc13 = LCChunk (CC "tree"
    (S "The implementation of the linked (tree) data structure."))
    [mod_linked]

lc14 = LCChunk (CC "hash table"
    (S "The implementation of the associative (hash table) data structure."))
    [mod_assoc]

-- Unlikely Changes --

uc1, uc2, uc3, uc4, uc5, uc6, uc7 :: UCChunk

uc1 = CC "IO"
    (S "Input/Output devices (Input: File and/or Keyboard, Output: File, " :+:
    S "Memory, and/or Screen).")

uc2 = CC "inputsource"
    (S "There will always be a source of input data external to the software.")

uc3 = CC "output" (S "Output data are displayed to the output device.")

uc4 = CC "goal"
    (S "The goal of the system is to simulate the interactions of 2D rigid " :+:
    S "bodies.")

uc5 = CC "Cartesian" (S "A Cartesian coordinate system is used.")

uc6 = CC "rigid" (S "All objects are rigid bodies.")

uc7 = CC "2D" (S "All objects are 2D.")
