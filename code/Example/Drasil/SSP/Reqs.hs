module Drasil.SSP.Reqs where

import Language.Drasil
import Drasil.SSP.Modules

reqs :: [ReqChunk]
reqs = [r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11]

r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11 :: ReqChunk
--FIXME: Why are these empty?
r1  = ReqChunk emptyN [mod_ctrl, mod_inputf, mod_sds]
r2  = ReqChunk emptyN [mod_genalg, mod_sds, mod_rng]
r3  = ReqChunk emptyN [mod_kinadm, mod_sds]
r4  = ReqChunk emptyN [mod_genalg, mod_slipslicer, mod_sds]
r5  = ReqChunk emptyN [mod_mp, mod_sds]
r6  = ReqChunk emptyN [mod_slipweight, mod_sds]
r7  = ReqChunk emptyN [mod_genalg, mod_sds, mod_rng]
r8  = ReqChunk emptyN [mod_genalg, mod_sds]
r9  = ReqChunk emptyN [mod_ctrl, mod_outputf, mod_slipslicer, mod_sds]
r10 = ReqChunk emptyN [mod_outputf, mod_mp, mod_rfem, mod_sds]
r11 = ReqChunk emptyN [mod_outputf, mod_plot]

emptyN :: NWrapper
emptyN = nw $ npnc "" (cn "")
