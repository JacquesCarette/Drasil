module Drasil.NoPCM.Definitions where --whole file is used

import Language.Drasil

--Common Terms
ht_trans :: NamedChunk
ht_trans = nc "heat transfer" (cn "heat transfer") --Not really a nounphase,
                                                   --just a hack to get RefSec to work

srs_swhs :: CommonConcept -- Used to make the title of the paper
srs_swhs = dcc' "srs_swhs" (nounPhraseSP "Solar Water Heating Systems")
  "SWHS" "SWHS"