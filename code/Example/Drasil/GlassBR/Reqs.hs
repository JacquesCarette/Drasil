module Drasil.GlassBR.Reqs where

import Language.Drasil
import Drasil.GlassBR.Modules

reqs :: [ReqChunk]
reqs = [r1,r2,r3,r4,r5,r6]

r1,r2,r3,r4,r5,r6 :: ReqChunk

-- FIXME: having id "" and term "" is completely bogus, and should not
-- be allowed.  This implicitly says that something here does not make sense.
r1 = ReqChunk (CC "" $ S "") [mod_hw, mod_inputf, mod_inputp, mod_ctrl]
r2 = ReqChunk (CC "" $ S "") [mod_inputf, mod_inputp]
r3 = ReqChunk (CC "" $ S "") [mod_inputc]
r4 = ReqChunk (CC "" $ S "") [mod_outputf]
r5 = ReqChunk (CC "" $ S "") [mod_outputf, mod_calc]
r6 = ReqChunk (CC "" $ S "") [mod_outputf]
