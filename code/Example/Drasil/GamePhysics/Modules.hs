module Drasil.GamePhysics.Modules where

import Language.Drasil
import Data.Drasil.Concepts.Software

import Drasil.GamePhysics.Concepts

modules :: [ModuleChunk]
modules = [mod_hw, mod_behav, mod_body, mod_shape, mod_circle, mod_segment,
    mod_poly, mod_space, mod_arbiter, mod_control, mod_sw, mod_vector, mod_bb,
    mod_trans, mod_spatial, mod_coll, mod_seq, mod_linked, mod_assoc]

-- M1: Hardware Hiding Module --

mod_hw :: ModuleChunk
mod_hw = makeImpModule modHWHiding
    (S "The data structure and algorithm used to implement the virtual " :+:
    S "hardware.")
    os
    []
    []
    []
    Nothing

-- Behaviour Hiding Module --

mod_behav :: ModuleChunk
mod_behav = makeUnimpModule modBehavHiding
    (S "The contents of the required behaviors.")
    Nothing

-- M2: Rigid Body Module --

mod_body_serv :: NamedChunk
mod_body_serv = CC "rigid body"
    (S "Stores the physical properties of an object, such as mass, " :+:
    S "position, rotation, velocity, etc, and provides operations on rigid " :+:
    S "bodies, such as setting the mass and velocity of the body.")

mod_body :: ModuleChunk
mod_body = makeImpModule mod_body_serv
    (S "The data structure of a rigid body.")
    chipmunk
    []
    []
    [mod_spatial, mod_trans, mod_vector, mod_space]
    (Just mod_behav)

-- M3: Shape Module --

mod_shape_serv :: NamedChunk
mod_shape_serv = CC "shape"
    (S "Stores the surface properties of an object, such as friction or " :+:
    S "elasticity, and provides operations on shapes, such as setting its " :+:
    S "friction or elasticity.")

mod_shape :: ModuleChunk
mod_shape = makeImpModule mod_shape_serv
    (S "The data structure of a collision shape. Children: Circle Module, " :+:
    S "Segment Module, Polygon Module.")
    chipmunk
    []
    []
    [mod_trans, mod_bb, mod_vector, mod_body, mod_space]
    (Just mod_behav)

-- M4, M5, M6: Circle, Segment, Polygon Modules (M3 submodules) --

mod_circle_serv, mod_segment_serv, mod_poly_serv :: NamedChunk
mod_circle_serv = CC "circle"
    (S "Provides operations on circles such as initializing a new circle, " :+:
    S "calculating moment and area, etc.")
mod_segment_serv = CC "segment"
    (S "Provides operations on segments such as initializing a new " :+:
    S "segment, calculating moment and area, etc.")
mod_poly_serv = CC "poly"
    (S "Provides operations on polygons such as initializing a new " :+:
    S "polygon, calculating moment, area and centroid, etc.")

mod_circle :: ModuleChunk
mod_circle = makeImpModule mod_circle_serv
    (S "The data structure for a circle shape.")
    chipmunk
    []
    []
    []
    (Just mod_shape)

mod_segment :: ModuleChunk
mod_segment = makeImpModule mod_segment_serv
    (S "The data structure for a segment shape.")
    chipmunk
    []
    []
    []
    (Just mod_shape)

mod_poly :: ModuleChunk
mod_poly = makeImpModule mod_poly_serv
    (S "The data structure for a polygon shape.")
    chipmunk
    []
    []
    []
    (Just mod_shape)

-- M7: Space Module --

mod_space_serv :: NamedChunk
mod_space_serv = CC "space"
    (S "Controls how all the rigid bodies and shapes interact together.")

mod_space :: ModuleChunk
mod_space = makeImpModule mod_space_serv
    (S "The container for simulating objects.")
    chipmunk
    []
    []
    [mod_bb, mod_spatial, mod_assoc, mod_seq, mod_spatial]
    (Just mod_behav)

-- M8: Arbiter Module --

mod_arbiter_serv :: NamedChunk
mod_arbiter_serv = CC "arbiter"
    (S "Stores all collision data, such as which bodies collided and " :+:
    S "their masses.")

mod_arbiter :: ModuleChunk
mod_arbiter = makeImpModule mod_arbiter_serv
    (S "The data structure containing collision information.")
    chipmunk
    []
    []
    [mod_shape, mod_body, mod_vector]
    (Just mod_behav)

-- M9: Control Module --

mod_control :: ModuleChunk
mod_control = makeImpModule modControl
    (S "The internal data types and algorithms for coordinating the " :+:
    S "running of the program.")
    chipmunk
    []
    []
    [mod_arbiter, mod_hw]
    (Just mod_behav)

-- Software Decision Module --

mod_sw :: ModuleChunk
mod_sw = makeUnimpModule modSfwrDecision
    (S "The design decision based on mathematical theorems, physical facts" :+:
    S ", or programming considerations. The secrets of this module are " :+:
    S "not described in the SRS.")
    Nothing

-- M10: Vector Module --

mod_vector_serv :: NamedChunk
mod_vector_serv = CC "vector"
    (S "Provides vector operations such as addition, scalar and vector " :+:
    S "multiplication, dot and cross products, rotations, etc.")

mod_vector :: ModuleChunk
mod_vector = makeImpModule mod_vector_serv
    (S "The data structure representing vectors.")
    chipmunk
    []
    []
    []
    (Just mod_sw)

-- M11: Bounding Box Module --

mod_bb_serv :: NamedChunk
mod_bb_serv = CC "bounding box"
    (S "Provides constructors for bounding boxes and operations such as " :+:
    S "merging boxes, calculating their centroids and areas, etc.")

mod_bb :: ModuleChunk
mod_bb = makeImpModule mod_bb_serv
    (S "The data structure for representing axis-aligned bounding boxes.")
    chipmunk
    []
    []
    [mod_vector]
    (Just mod_sw)

-- M12: Transform Matrix Module --

mod_trans_serv :: NamedChunk
mod_trans_serv = CC "transform matrix"
    (S "Provides constructors for affine transformation matrices, matrix " :+:
    S "operations such as inverse, transpose, multiplications, and " :+:
    S "operations for applying transformations to vectors and bounding boxes.")

mod_trans :: ModuleChunk
mod_trans = makeImpModule mod_trans_serv
    (S "The data structure representing transformation matrices.")
    chipmunk
    []
    []
    [mod_bb]
    (Just mod_sw)

-- M13: Spatial Index Module --

mod_spatial_serv :: NamedChunk
mod_spatial_serv = CC "spatial index"
    (S "Provides spatial indexing operations and tracks the positions of " :+:
    S "bodies in the simulation space.")

mod_spatial :: ModuleChunk
mod_spatial = makeImpModule mod_spatial_serv
    (S "The data structures and algorithms for detecting collisions.")
    chipmunk
    []
    []
    [mod_bb, mod_vector, mod_coll, mod_linked]
    (Just mod_sw)

-- M14: Collision Solver Module --

mod_coll_serv :: NamedChunk
mod_coll_serv = CC "collision solver"
    (S "Fast collision filtering, primitive shape-to-shape collision " :+:
    S "detection.")

mod_coll :: ModuleChunk
mod_coll = makeImpModule mod_coll_serv
    (S "The data structures and algorithms for detecting collisions.")
    chipmunk
    []
    []
    [mod_bb, mod_vector, mod_linked]
    (Just mod_sw)

-- M15: Sequence Data Structure Module --

mod_seq_serv :: NamedChunk
mod_seq_serv = CC "sequence data structure"
    (S "Provides array manipulation operations, such as building an array " :+:
    S ", accessing a specific entry, slicing an array, etc.")

mod_seq :: ModuleChunk
mod_seq = makeImpModule mod_seq_serv
    (S "The data structure for a sequence data type.")
    chipmunk
    []
    []
    []
    (Just mod_sw)

-- M16: Linked Data Structure Module --

mod_linked_serv :: NamedChunk
mod_linked_serv = CC "linked data structure"
    (S "Provides tree manipulation operations, such as building a tree, " :+:
    S "accessing a specific entry, etc.")

mod_linked :: ModuleChunk
mod_linked = makeImpModule mod_linked_serv
    (S "The data structure for a linked data type.")
    chipmunk
    []
    []
    []
    (Just mod_sw)

-- M17: Associative Data Structure Module --

mod_assoc_serv :: NamedChunk
mod_assoc_serv = CC "associative data structure"
    (S "Provides operations on hash tables, such as building a hash table, " :+:
    S "accessing a specific entry, etc.")

mod_assoc :: ModuleChunk
mod_assoc = makeImpModule mod_assoc_serv
    (S "The data structure for an associative data type.")
    chipmunk
    []
    []
    []
    (Just mod_sw)
