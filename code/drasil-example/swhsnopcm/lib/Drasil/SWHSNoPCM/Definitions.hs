module Drasil.SWHSNoPCM.Definitions where --whole file is used

import Language.Drasil

--Common Terms
htTrans :: IdeaDict
htTrans = nc "heat transfer" (cn "heat transfer") --Not really a nounphase,
                                                   --just a hack to get RefSec to work

srsSWHS :: CI -- Used to make the title of the paper
srsSWHS = commonIdea "srsSWHS" (nounPhraseSP "Solar Water Heating Systems") "SWHSNoPCM" []