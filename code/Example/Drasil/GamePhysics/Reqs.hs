module Drasil.GamePhysics.Reqs where

import Language.Drasil
import Drasil.GamePhysics.Modules

reqs :: [ReqChunk]
reqs = [r1,r2,r3,r4,r5,r6,r7,r8]

r1,r2,r3,r4,r5,r6,r7,r8 :: ReqChunk
--FIXME: Why are all of these empty? Makes no sense.
r1 = ReqChunk (emptyN) [mod_space, mod_ctrl, mod_seq]
r2 = ReqChunk (emptyN) [mod_body, mod_ctrl, mod_vector, mod_trans]
r3 = ReqChunk (emptyN) [mod_shape, mod_circle, mod_segment, mod_poly, 
  mod_ctrl, mod_vector]
r4 = ReqChunk (emptyN) [mod_body, mod_shape, mod_circle, mod_segment,
  mod_poly, mod_space, mod_ctrl]
r5 = ReqChunk (emptyN) [mod_body, mod_space, mod_vector, mod_trans]
r6 = ReqChunk (emptyN) [mod_body, mod_space, mod_vector, mod_trans]
r7 = ReqChunk (emptyN) [mod_body, mod_space, mod_bb, mod_spatial,
  mod_coll, mod_linked, mod_assoc]
r8 = ReqChunk (emptyN) [mod_body, mod_space, mod_arbiter, mod_vector,
  mod_trans]