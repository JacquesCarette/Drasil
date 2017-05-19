module Drasil.GlassBR.Reqs where

import Language.Drasil
import Drasil.GlassBR.Modules
import Data.Drasil.Modules

reqs :: [ReqChunk]
reqs = [req1, req2, req3, req4, req5, req6]

req1, req2, req3, req4, req5, req6 :: ReqChunk

req1 = ReqChunk (nw emptyN) [mod_hw, mod_inputf, mod_inputp, mod_ctrl]
req2 = ReqChunk (nw emptyN) [mod_inputf, mod_inputp]
req3 = ReqChunk (nw emptyN) [mod_inputc]
req4 = ReqChunk (nw emptyN) [mod_outputf]
req5 = ReqChunk (nw emptyN) [mod_outputf, mod_calc]
req6 = ReqChunk (nw emptyN) [mod_outputf]

emptyN :: NPNC
emptyN = npnc "" (cn "")
