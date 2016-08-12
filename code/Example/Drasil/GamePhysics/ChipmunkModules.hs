module Example.Drasil.GamePhysics.ChipmunkModules where

import Language.Drasil
import Example.Drasil.GamePhysics.ChipmunkConcepts

modules :: [ModuleChunk]
modules = [mod_hw, mod_behav, mod_body, mod_shape, mod_space, mod_arbiter,
    mod_control, mod_sw, mod_vector, mod_bb, mod_trans, mod_spatial, mod_coll,
    mod_seq, mod_linked, mod_assoc]

-- M1: Hardware Hiding Module --

mod_hw_desc :: ConceptChunk
mod_hw_desc = CC "hardware hiding"
    (S "Serves as a virtual hardware used by the rest of the system. This " :+:
    S "module provides the interface between the hardware and the software." :+:
    S " So, the system can use it to display outputs or to accept inputs.")

mod_hw :: ModuleChunk
mod_hw = makeImpModule mod_hw_desc
    (S "The data structure and algorithm used to implement the virtual " :+:
    S "hardware.")
    os
    []
    Nothing

-- Behaviour Hiding Module --

mod_behav_desc :: ConceptChunk
mod_behav_desc = CC "behaviour hiding"
    (S "Includes programs that provide externally visible behavior of " :+:
    S "the system as specified in the software requirements specification " :+:
    S "(SRS) documents. This module serves as a communication layer " :+:
    S "between the hardware-hiding module and the software decision " :+:
    S "module. The programs in this module will need to change if there " :+:
    S "are changes in the SRS.")

mod_behav :: ModuleChunk
mod_behav = makeUnimpModule mod_behav_desc
    (S "The contents of the required behaviors.")
    Nothing

-- M2: Rigid Body Module --

mod_body_desc :: ConceptChunk
mod_body_desc = CC "rigid body"
    (S "Stores the physical properties of an object, such as mass, " :+:
    S "position, rotation, velocity, etc, and provides operations on rigid " :+:
    S "bodies, such as setting the mass and velocity of the body.")

mod_body :: ModuleChunk
mod_body = makeImpModule mod_body_desc
    (S "The data structure of a rigid body.")
    chipmunk
    []
    (Just mod_behav)

-- M3: Shape Module --

mod_shape_desc :: ConceptChunk
mod_shape_desc = CC "shape"
    (S "Stores the surface properties of an object, such as friction or " :+:
    S "elasticity, and provides operations on shapes, such as setting its " :+:
    S "friction or elasticity.")

mod_shape :: ModuleChunk
mod_shape = makeImpModule mod_shape_desc
    (S "The data structure of a collision shape. Children: Circle Module, " :+:
    S "Segment Module, Polygon Module.")
    chipmunk
    []
    (Just mod_behav)

-- M4: Space Module --

mod_space_desc :: ConceptChunk
mod_space_desc = CC "space"
    (S "Controls how all the rigid bodies and shapes interact together.")

mod_space :: ModuleChunk
mod_space = makeImpModule mod_space_desc
    (S "The container for simulating objects.")
    chipmunk
    []
    (Just mod_behav)

-- M5: Arbiter Module --

mod_arbiter_desc :: ConceptChunk
mod_arbiter_desc = CC "arbiter"
    (S "Stores all collision data, such as which bodies collided and " :+:
    S "their masses.")

mod_arbiter :: ModuleChunk
mod_arbiter = makeImpModule mod_arbiter_desc
    (S "The data structure containing collision information.")
    chipmunk
    []
    (Just mod_behav)

-- M6: Control Module --

mod_control_desc :: ConceptChunk
mod_control_desc = CC "control"
    (S "Provides the main program.")

mod_control :: ModuleChunk
mod_control = makeImpModule mod_control_desc
    (S "The internal data types and algorithms for coordinating the " :+:
    S "running of the program.")
    chipmunk
    []
    (Just mod_behav)

-- Software Decision Module --

mod_sw_desc :: ConceptChunk
mod_sw_desc = CC "software decision"
    (S "Includes data structures and algorithms used in the system that " :+:
    S "do not provide direct interaction with the user.")

mod_sw :: ModuleChunk
mod_sw = makeUnimpModule mod_sw_desc
    (S "The design decision based on mathematical theorems, physical facts" :+:
    S ", or programming considerations. The secrets of this module are " :+:
    S "not described in the SRS.")
    Nothing

-- M7: Vector Module --

mod_vector_desc :: ConceptChunk
mod_vector_desc = CC "vector"
    (S "Provides vector operations such as addition, scalar and vector " :+:
    S "multiplication, dot and cross products, rotations, etc.")

mod_vector :: ModuleChunk
mod_vector = makeImpModule mod_vector_desc
    (S "The data structure representing vectors.")
    chipmunk
    []
    (Just mod_sw)

-- M8: Bounding Box Module --

mod_bb_desc :: ConceptChunk
mod_bb_desc = CC "bounding box"
    (S "Provides constructors for bounding boxes and operations such as " :+:
    S "merging boxes, calculating their centroids and areas, etc.")

mod_bb :: ModuleChunk
mod_bb = makeImpModule mod_bb_desc
    (S "The data structure for representing axis-aligned bounding boxes.")
    chipmunk
    []
    (Just mod_sw)

-- M9: Transform Matrix Module --

mod_trans_desc :: ConceptChunk
mod_trans_desc = CC "transform matrix"
    (S "Provides constructors for affine transformation matrices, matrix " :+:
    S "operations such as inverse, transpose, multiplications, and " :+:
    S "operations for applying transformations to vectors and bounding boxes.")

mod_trans :: ModuleChunk
mod_trans = makeImpModule mod_trans_desc
    (S "The data structure representing transformation matrices.")
    chipmunk
    []
    (Just mod_sw)

-- M10: Spatial Index Module --

mod_spatial_desc :: ConceptChunk
mod_spatial_desc = CC "spatial index"
    (S "Provides spatial indexing operations and tracks the positions of " :+:
    S "bodies in the simulation space.")

mod_spatial :: ModuleChunk
mod_spatial = makeImpModule mod_spatial_desc
    (S "The data structures and algorithms for detecting collisions.")
    chipmunk
    []
    (Just mod_sw)

-- M11: Collision Solver Module --

mod_coll_desc :: ConceptChunk
mod_coll_desc = CC "collision solver"
    (S "Fast collision filtering, primitive shape-to-shape collision " :+:
    S "detection.")

mod_coll :: ModuleChunk
mod_coll = makeImpModule mod_coll_desc
    (S "The data structures and algorithms for detecting collisions.")
    chipmunk
    []
    (Just mod_sw)

-- M12: Sequence Data Structure Module --

mod_seq_desc :: ConceptChunk
mod_seq_desc = CC "sequence data structure"
    (S "Provides array manipulation operations, such as building an array " :+:
    S ", accessing a specific entry, slicing an array, etc.")

mod_seq :: ModuleChunk
mod_seq = makeImpModule mod_seq_desc
    (S "The data structure for a sequence data type.")
    chipmunk
    []
    (Just mod_sw)

-- M13: Linked Data Structure Module --

mod_linked_desc :: ConceptChunk
mod_linked_desc = CC "linked data structure"
    (S "Provides tree manipulation operations, such as building a tree, " :+:
    S "accessing a specific entry, etc.")

mod_linked :: ModuleChunk
mod_linked = makeImpModule mod_linked_desc
    (S "The data structure for a linked data type.")
    chipmunk
    []
    (Just mod_sw)

-- M14: Associative Data Structure Module --

mod_assoc_desc :: ConceptChunk
mod_assoc_desc = CC "associative data structure"
    (S "Provides operations on hash tables, such as building a hash table, " :+:
    S "accessing a specific entry, etc.")

mod_assoc :: ModuleChunk
mod_assoc = makeImpModule mod_assoc_desc
    (S "The data structure for an associative data type.")
    chipmunk
    []
    (Just mod_sw)
