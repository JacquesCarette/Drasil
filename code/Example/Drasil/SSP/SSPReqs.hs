module Drasil.SSP.SSPReqs where

import Language.Drasil
import Drasil.SSP.SSPModules

reqs :: [ReqChunk]
reqs = [r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11]

r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11 :: ReqChunk

r1 = ReqChunk (CC "" $ S "") [mod_ctrl, mod_inputf, mod_sds]
r2 = ReqChunk (CC "" $ S "") [mod_genalg, mod_sds, mod_rng]
r3 = ReqChunk (CC "" $ S "") [mod_kinadm, mod_sds]
r4 = ReqChunk (CC "" $ S "") [mod_genalg, mod_slipslicer, mod_sds]
r5 = ReqChunk (CC "" $ S "") [mod_mp, mod_sds]
r6 = ReqChunk (CC "" $ S "") [mod_slipweight, mod_sds]
r7 = ReqChunk (CC "" $ S "") [mod_genalg, mod_sds, mod_rng]
r8 = ReqChunk (CC "" $ S "") [mod_genalg, mod_sds]
r9 = ReqChunk (CC "" $ S "") [mod_ctrl, mod_outputf, mod_slipslicer, mod_sds]
r10 = ReqChunk (CC "" $ S "") [mod_outputf, mod_mp, mod_rfem, mod_sds]
r11 = ReqChunk (CC "" $ S "") [mod_outputf, mod_plot]
