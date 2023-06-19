module Drasil.NoPCM.Definitions where --whole file is used

import Language.Drasil

--Common Terms
htTrans :: IdeaDict
htTrans = nc "heat transfer" (cn "heat transfer") --Not really a nounphase,
                                                   --just a hack to get RefSec to work

srsSWHS :: CommonConcept -- Used to make the title of the paper
srsSWHS = dcc' "srsSWHS" (nounPhraseSP "Solar Water Heating Systems")
  "SWHS" "NoPCM"