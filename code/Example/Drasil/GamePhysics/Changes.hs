module Drasil.GamePhysics.Changes where

import Language.Drasil
import Drasil.GamePhysics.Modules
import Data.Drasil.Changes

likelyChanges :: [LCChunk]
likelyChanges = [hardware, rigidBodyStruct, shapeLC, spaceLC, arbiterLC, control, 
  vectorStruct, boundingBox, transform, spatialIndex, collisionAlgo, sequenceStruct, 
  linkedListStruct, hashtableStruct]

unlikelyChanges :: [UCChunk]
unlikelyChanges = [ucIO, ucInputS, ucOutput, ucGoal, ucCart, ucRigid, uc2D]

-- Likely Changes --

hardware, rigidBodyStruct, shapeLC, spaceLC, arbiterLC, control, vectorStruct, 
  boundingBox, transform, spatialIndex, collisionAlgo, sequenceStruct, 
  linkedListStruct, hashtableStruct:: LCChunk

hardware = lcHW

rigidBodyStruct = LCChunk (nw $ npnc "rigid body"
  (nounPhraseSP $ 
  "The data structure of the physical properties of an object such as " ++ 
  "the object's mass, position and velocity."))
  [mod_body]

shapeLC = LCChunk (nw $ npnc "shape"
  (nounPhraseSP $ 
  "The data structure of the surface properties of an object such as " ++
  "the object's friction and elasticity."))
  [mod_shape, mod_circle, mod_segment, mod_poly]

spaceLC = LCChunk (nw $ npnc "space"
  (nounPhraseSP $ "How all the rigid bodies and shapes interact together."))
  [mod_space]

arbiterLC = LCChunk (nw $ npnc "arbiter" (nounPhraseSP $ 
  "The data structure containing collision information such as the " ++
  "objects that collide and their masses."))
  [mod_arbiter]

control = LCChunk (nw $ npnc "control" (nounPhraseSP $ 
  "How the overall control of the simulation is orchestrated, " ++
  "including the input and output of data."))
  [mod_ctrl]

vectorStruct = lcVect mod_vector

boundingBox = LCChunk (nw $ npnc "bb" (nounPhraseSP $ 
  "The implementation of bounding box structures"))
  [mod_bb]

transform = LCChunk (nw $ npnc "transform" (nounPhraseSP $ 
  "The implementation of affine transformation matrices."))
  [mod_vector]

spatialIndex = LCChunk (nw $ npnc "spatial index" (nounPhraseSP $ 
  "How the simulation space is spatially indexed."))
  [mod_spatial]

collisionAlgo = LCChunk (nw $ npnc "collision" (nounPhraseSP $ 
  "The algorithms used for solving collisions."))
  [mod_coll]

sequenceStruct = lcArray mod_seq

linkedListStruct = lcTree mod_linked

hashtableStruct = lcHash mod_assoc

-- Unlikely Changes --

ucGoal, ucRigid :: UCChunk

ucGoal = nw $ npnc "goal" (nounPhraseSP $ 
  "The goal of the system is to simulate the interactions of 2D rigid bodies.")

ucRigid = nw $ npnc "rigid" (nounPhraseSP "All objects are rigid bodies.")

