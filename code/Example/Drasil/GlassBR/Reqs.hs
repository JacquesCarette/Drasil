module Drasil.GlassBR.Reqs (reqs) where

import Language.Drasil (ReqChunk (ReqChunk), emptyN)

import Drasil.GlassBR.Modules (mod_inputf, mod_inputp, mod_ctrl,
  mod_inputc, mod_outputf, mod_calc)

reqs :: [ReqChunk]
reqs = [req1, req2, req3, req4, req5, req6]

req1, req2, req3, req4, req5, req6 :: ReqChunk

req1 = ReqChunk (emptyN) [mod_inputf, mod_inputp, mod_ctrl]
req2 = ReqChunk (emptyN) [mod_inputf, mod_inputp]
req3 = ReqChunk (emptyN) [mod_inputc]
req4 = ReqChunk (emptyN) [mod_outputf]
req5 = ReqChunk (emptyN) [mod_outputf, mod_calc]
req6 = ReqChunk (emptyN) [mod_outputf]

--FIXME: Why are all of these empty? Makes no sense.