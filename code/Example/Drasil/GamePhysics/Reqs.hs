module Drasil.GamePhysics.Reqs where

import Language.Drasil
import Drasil.GamePhysics.Modules

reqs :: [ReqChunk]
reqs = [r1,r2,r3,r4,r5,r6,r7,r8]

r1,r2,r3,r4,r5,r6,r7,r8 :: ReqChunk

r1 = ReqChunk (CC "" $ S "") [mod_space, mod_control, mod_seq]
r2 = ReqChunk (CC "" $ S "") [mod_body, mod_control, mod_vector, mod_trans]
r3 = ReqChunk (CC "" $ S "") [mod_shape, mod_circle, mod_segment, mod_poly,
  mod_control, mod_vector]
r4 = ReqChunk (CC "" $ S "") [mod_body, mod_shape, mod_circle, mod_segment,
  mod_poly, mod_space, mod_control]
r5 = ReqChunk (CC "" $ S "") [mod_body, mod_space, mod_vector, mod_trans]
r6 = ReqChunk (CC "" $ S "") [mod_body, mod_space, mod_vector, mod_trans]
r7 = ReqChunk (CC "" $ S "") [mod_body, mod_space, mod_bb, mod_spatial,
  mod_coll, mod_linked, mod_assoc]
r8 = ReqChunk (CC "" $ S "") [mod_body, mod_space, mod_arbiter, mod_vector,
  mod_trans]
