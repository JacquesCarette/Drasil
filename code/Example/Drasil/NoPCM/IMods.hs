module Drasil.NoPCM.IMods (eBalanceOnWtr) where

import Language.Drasil
import Drasil.DocumentLanguage (mkAssump)


import Drasil.SWHS.Concepts (water)
import Drasil.SWHS.Unitals
import Data.Drasil.Utils (unwrap)
import Data.Drasil.SentenceStructures (foldlSent, isThe,
  sAnd)
import Data.Drasil.Quantities.Physics (time)
import Data.Drasil.Concepts.Math (equation)
import Data.Drasil.Concepts.PhysicalProperties (liquid)
import Data.Drasil.Concepts.Thermodynamics (melting, boil_pt)

---------
-- IM1 --
---------
eBalanceOnWtr :: RelationConcept
eBalanceOnWtr = makeRC "eBalanceOnWtr" (nounPhraseSP $ "Energy balance on " ++
  "water to find the temperature of the water") balWtrDesc balWtr_Rel

balWtr_Rel :: Relation
balWtr_Rel = (Deriv Total (sy temp_W) time) $= (Int 1) / (sy tau_W) *
  (((sy temp_C) - (FCall (sy temp_W) [sy time])))

balWtrDesc :: Sentence
balWtrDesc = foldlSent [(E $ sy temp_W) `isThe` phrase temp_W +:+.
  sParen (unwrap $ getUnit temp_W), 
  (E $ sy temp_C) `isThe` phrase temp_C +:+. sParen (unwrap $ getUnit temp_C),
  (E $ sy tau_W $= (sy w_mass * sy htCap_W) / (sy coil_HTC * sy coil_SA)),
  S "is a constant" +:+. sParen (unwrap $ getUnit tau_W),
  S "The above", phrase equation, S "applies as long as the", phrase water,
  S "is in", phrase liquid, S "form" `sC` (E $ Int 0 $< sy temp_W $< (Int 100)),
  sParen (unwrap $ getUnit temp_W), S "where", S $ show (0 :: Integer),
  sParen (unwrap $ getUnit temp_W) `sAnd` (S $ show (100 :: Integer)),
  sParen (unwrap $ getUnit temp_W), S "are the", phrase melting `sAnd`
  plural boil_pt, S "of", phrase water `sC` S "respectively",
  sParen (makeRef (mkAssump "assump10" EmptyS))]
  
