module Drasil.GamePhysics.Modules where

import Control.Lens ((^.))
import Language.Drasil
import Data.Drasil.SentenceStructures (foldlSent, foldlList)
import Data.Drasil.Concepts.Physics (rigidBody, velocity, position, friction, 
    elasticity)
import Data.Drasil.Quantities.PhysicalProperties (mass)
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.Computation
import Data.Drasil.Modules

import Drasil.GamePhysics.Concepts


mod_vector, mod_seq, mod_assoc, mod_linked, mod_ctrl :: ModuleChunk

mod_vector = mod_vector_fun chipmunk []
mod_seq    = mod_seq_fun chipmunk []
mod_assoc  = mod_assoc_fun chipmunk []
mod_linked = mod_linked_fun chipmunk []
mod_ctrl   = mod_ctrl_fun (S "The internal" +:+ (plural $ dataType' ^. term) +:+ 
  S "and" +:+ (plural $ algorithm ^. term))
  chipmunk [] [mod_arbiter, mod_hw]

modules :: [ModuleChunk]
modules = [mod_hw, mod_behav, mod_body, mod_shape, mod_circle, mod_segment,
    mod_poly, mod_space, mod_arbiter, mod_ctrl, mod_sw, mod_vector, mod_bb, 
    mod_trans, mod_spatial, mod_coll, mod_seq, mod_linked, mod_assoc]

-- M1: Hardware Hiding Module --

-- Behaviour Hiding Module --

-- M2: Rigid Body Module --

mod_body_serv :: ConceptChunk
mod_body_serv = dccWDS "mod_body_serv" (cnIES "rigid body")
    (foldlSent [S "Stores the", (plural physicalProperty), 
    S "of an object, such as", (foldlList [(phrase $ mass ^. term), 
    (phrase $ position ^. term), S "rotation", 
    (phrase $ velocity ^. term), S "etc", S "provides operations on"]), 
    (plural $ rigidBody ^. term) `sC` S "such as setting the", 
    (phrase $ mass ^. term), S "and", 
    (phrase $ velocity ^. term), S "of the body"])

mod_body :: ModuleChunk
mod_body = makeImpModule mod_body_serv
    (S "The" +:+ (plural $ dataStruct) +:+ S "of a" 
    +:+. (phrase $ rigidBody ^. term))
    chipmunk
    []
    []
    [mod_spatial, mod_trans, mod_vector, mod_space]
    (Just mod_behav)

-- M3: Shape Module --

mod_shape_serv :: ConceptChunk
mod_shape_serv = dccWDS "mod_shape_serv" (cn' "shape")
    (foldlSent [S "Stores the surface", (plural $ property ^. term), 
    S "of an object, such as", (phrase $ friction ^. term), S "or",
    (phrase $ elasticity ^. term) `sC`
    S "and provides operations on shapes, such as setting its",
    (phrase $ friction ^. term), S "or", (phrase $ elasticity ^. term)])

mod_shape :: ModuleChunk
mod_shape = makeImpModule mod_shape_serv
    (foldlSent [S "The" +:+ (plural $ dataStruct) +:+ 
    S "of a collision shape. Children: Circle", 
    (titleize module_) `sC` S "Segment", (titleize module_) `sC` 
    S "Polygon", (titleize module_)])
    chipmunk
    []
    []
    [mod_trans, mod_bb, mod_vector, mod_body, mod_space]
    (Just mod_behav)

-- M4, M5, M6: Circle, Segment, Polygon Modules (M3 submodules) --

mod_circle_serv, mod_segment_serv, mod_poly_serv :: ConceptChunk
mod_circle_serv = dccWDS "mod_circle_serv" (cn' "circle")
    (S "Provides operations on circles such as initializing a new circle," +:+.
    S "calculating moment and area, etc")

mod_segment_serv = dccWDS "mod_segment_serv" (cn' "segment")
    (S "Provides operations on segments such as initializing a new" +:+.
    S "segment, calculating moment and area, etc")

mod_poly_serv = dccWDS "mod_poly_serv" (cn' "poly")
    (S "Provides operations on polygons such as initializing a new" +:+.
    S "polygon, calculating moment, area and centroid, etc")

mod_circle :: ModuleChunk
mod_circle = makeImpModule mod_circle_serv
    (S "The" +:+ (plural $ dataStruct) +:+ S "for a circle shape.")
    chipmunk
    []
    []
    []
    (Just mod_shape)

mod_segment :: ModuleChunk
mod_segment = makeImpModule mod_segment_serv
    (S "The" +:+ (plural $ dataStruct) +:+ S "for a segment shape.")
    chipmunk
    []
    []
    []
    (Just mod_shape)

mod_poly :: ModuleChunk
mod_poly = makeImpModule mod_poly_serv
    (S "The" +:+ (plural $ dataStruct) +:+ S "for a polygon shape.")
    chipmunk
    []
    []
    []
    (Just mod_shape)

-- M7: Space Module --

mod_space_serv :: ConceptChunk
mod_space_serv = dccWDS "mod_space_serv" (cn' "space")
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

mod_arbiter_serv :: ConceptChunk
mod_arbiter_serv = dccWDS "mod_arbiter_serv" (cn' "arbiter")
    (S "Stores all collision data, such as which bodies collided and" +:+. 
    S "their masses")

mod_arbiter :: ModuleChunk
mod_arbiter = makeImpModule mod_arbiter_serv
    (S "The" +:+ (plural $ dataStruct) +:+ S "containing collision information.")
    chipmunk
    []
    []
    [mod_shape, mod_body, mod_vector]
    (Just mod_behav)

-- M9: Control Module --

-- Software Decision Module --

-- M10: Vector Module --

-- M11: Bounding Box Module --

mod_bb_serv :: ConceptChunk
mod_bb_serv = dccWDS "mod_bb_serv" (cn''' "bounding box")
    (S "Provides constructors for bounding boxes and operations such as " :+:
    S "merging boxes, calculating their centroids and areas, etc.")

mod_bb :: ModuleChunk
mod_bb = makeImpModule mod_bb_serv
    (S "The" +:+ (plural $ dataStruct) +:+ 
    S "for representing axis-aligned bounding boxes.")
    chipmunk
    []
    []
    [mod_vector]
    (Just mod_sw)

-- M12: Transform Matrix Module --

mod_trans_serv :: ConceptChunk
mod_trans_serv = dccWDS "mod_trans_serv" (nounPhraseSP "transform matrix")
    (S "Provides constructors for affine transformation matrices, matrix " :+:
    S "operations such as" +:+ (foldlList [S "inverse", S "transpose", 
    S "multiplications", S "operations"]) +:+ 
    S "for applying transformations to vectors and bounding boxes.")

mod_trans :: ModuleChunk
mod_trans = makeImpModule mod_trans_serv
    (S "The" +:+ (plural $ dataStruct) +:+ S "representing transformation matrices.")
    chipmunk
    []
    []
    [mod_bb]
    (Just mod_sw)

-- M13: Spatial Index Module --

mod_spatial_serv :: ConceptChunk
mod_spatial_serv = dccWDS "mod_spatial_serv" (nounPhraseSP "spatial index")
    (S "Provides spatial indexing operations and tracks the positions of " :+:
    S "bodies in the simulation space.")

mod_spatial :: ModuleChunk
mod_spatial = makeImpModule mod_spatial_serv
    (foldlSent [S "The", (plural $ dataStruct'), S "and", 
    (plural $ algorithm ^. term), S "for detecting collisions"])
    chipmunk
    []
    []
    [mod_bb, mod_vector, mod_coll, mod_linked]
    (Just mod_sw)

-- M14: Collision Solver Module --

mod_coll_serv :: ConceptChunk
mod_coll_serv = dccWDS "mod_coll_serv" (cn' "collision solver")
    (S "Fast collision filtering, primitive shape-to-shape collision " :+:
    S "detection.")

mod_coll :: ModuleChunk
mod_coll = makeImpModule mod_coll_serv
    (foldlSent [S "The", (plural $ dataStruct'), S "and", 
    (plural $ algorithm ^. term), S "for detecting collisions"])
    chipmunk
    []
    []
    [mod_bb, mod_vector, mod_linked]
    (Just mod_sw)

-- M15: Sequence Data Structure Module --

-- M16: Linked Data Structure Module --

-- M17: Associative Data Structure Module --
