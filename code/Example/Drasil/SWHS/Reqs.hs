module Drasil.SWHS.Reqs where

import Language.Drasil (NamedChunk,
  npnc, cn, nw,
  ReqChunk (ReqChunk))
import Drasil.SWHS.Modules (mod_ener, mod_ctrl, mod_outputf, mod_temp,
  mod_plot, mod_outputv, mod_seq, mod_ode, mod_inputv, mod_inputp,
  mod_inputf)
import Data.Drasil.Modules (mod_hw)

reqs :: [ReqChunk]
reqs = [r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11]

r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11 :: ReqChunk
--FIXME: Why are these all empty?
r1 = ReqChunk (nw $ emptyN) [mod_hw, mod_inputf, mod_inputp, mod_ctrl]
r2 = ReqChunk (nw $ emptyN) [mod_inputf, mod_inputp]
r3 = ReqChunk (nw $ emptyN) [mod_inputv]
r4 = ReqChunk (nw $ emptyN) [mod_outputf, mod_ctrl]
r5 = ReqChunk (nw $ emptyN) [mod_outputf, mod_temp, mod_ctrl, mod_seq, mod_ode,
  mod_plot]
r6 = ReqChunk (nw $ emptyN) [mod_outputf, mod_temp, mod_ctrl, mod_seq, mod_ode,
  mod_plot]
r7 = ReqChunk (nw $ emptyN) [mod_outputf, mod_ener, mod_ctrl, mod_seq, mod_plot]
r8 = ReqChunk (nw $ emptyN) [mod_outputf, mod_ener, mod_ctrl, mod_seq, mod_plot]
r9 = ReqChunk (nw $ emptyN) [mod_outputv]
r10 = ReqChunk (nw $ emptyN) [mod_outputf, mod_temp, mod_ctrl]
r11 = ReqChunk (nw $ emptyN) [mod_outputf, mod_temp, mod_ener, mod_ctrl]

emptyN :: NamedChunk
emptyN = npnc "" (cn "")