module Drasil.GamePhysics.Reqs where

import Language.Drasil
import Drasil.GamePhysics.Modules
import Data.Drasil.Modules
import Drasil.GamePhysics.Concepts (chipmunk)

reqs :: [ReqChunk]
reqs = [r1,r2,r3,r4,r5,r6,r7,r8]

r1,r2,r3,r4,r5,r6,r7,r8 :: ReqChunk
--FIXME: Why are all of these empty? Makes no sense.
r1 = ReqChunk (nw emptyN) [mod_space, mod_control, (mod_seq chipmunk [])]
r2 = ReqChunk (nw emptyN) [mod_body, mod_control, (mod_vector chipmunk []), mod_trans]
r3 = ReqChunk (nw emptyN) [mod_shape, mod_circle, mod_segment, mod_poly,
  mod_control, (mod_vector chipmunk [])]
r4 = ReqChunk (nw emptyN) [mod_body, mod_shape, mod_circle, mod_segment,
  mod_poly, mod_space, mod_control]
r5 = ReqChunk (nw emptyN) [mod_body, mod_space, (mod_vector chipmunk []), mod_trans]
r6 = ReqChunk (nw emptyN) [mod_body, mod_space, (mod_vector chipmunk []), mod_trans]
r7 = ReqChunk (nw emptyN) [mod_body, mod_space, mod_bb, mod_spatial,
  mod_coll, (mod_linked chipmunk []), (mod_assoc chipmunk [])]
r8 = ReqChunk (nw emptyN) [mod_body, mod_space, mod_arbiter, (mod_vector chipmunk []),
  mod_trans]

emptyN :: NPNC
emptyN = npnc "" (cn "")
