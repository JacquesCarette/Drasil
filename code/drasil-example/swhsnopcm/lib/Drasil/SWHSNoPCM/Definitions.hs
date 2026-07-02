module Drasil.SWHSNoPCM.Definitions (htTrans) where --whole file is used

import Drasil.Database (mkUid)
import Language.Drasil

--Common Terms
htTrans :: IdeaDict
htTrans = idea' (mkUid "heat transfer") (cn "heat transfer") -- Not really a nounphase,
                                                   --just a hack to get RefSec to work
