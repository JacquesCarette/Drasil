module Drasil.SSP.Reqs where

import Language.Drasil
import Drasil.SSP.Modules

reqs :: [ReqChunk]
reqs = [r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11]

r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11 :: ReqChunk

r1 = ReqChunk (ncWDS "" $ S "") [mod_ctrl, mod_inputf, mod_sds]
r2 = ReqChunk (ncWDS "" $ S "") [mod_genalg, mod_sds, mod_rng]
r3 = ReqChunk (ncWDS "" $ S "") [mod_kinadm, mod_sds]
r4 = ReqChunk (ncWDS "" $ S "") [mod_genalg, mod_slipslicer, mod_sds]
r5 = ReqChunk (ncWDS "" $ S "") [mod_mp, mod_sds]
r6 = ReqChunk (ncWDS "" $ S "") [mod_slipweight, mod_sds]
r7 = ReqChunk (ncWDS "" $ S "") [mod_genalg, mod_sds, mod_rng]
r8 = ReqChunk (ncWDS "" $ S "") [mod_genalg, mod_sds]
r9 = ReqChunk (ncWDS "" $ S "") [mod_ctrl, mod_outputf, mod_slipslicer, mod_sds]
r10 = ReqChunk (ncWDS "" $ S "") [mod_outputf, mod_mp, mod_rfem, mod_sds]
r11 = ReqChunk (ncWDS "" $ S "") [mod_outputf, mod_plot]
