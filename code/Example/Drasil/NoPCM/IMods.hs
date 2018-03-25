module Drasil.NoPCM.IMods (eBalanceOnWtr, eBalanceOnWtr_new) where

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
import Drasil.DocumentLanguage.Chunk.InstanceModel
---------
-- IM1 --
---------
--im :: RelationConcept -> Inputs -> InputConstraints -> Outputs -> 
-- OutputConstraints -> Attributes -> InstanceModel
--Tcon :: Expr -> Constraint
eBalanceOnWtr_new :: InstanceModel
eBalanceOnWtr_new = im eBalanceOnWtr [qw temp_C, qw temp_init, qw time_final, 
  qw coil_SA, qw coil_HTC, qw htCap_W, qw w_mass] 
  [TCon AssumedCon $C temp_init $<= C temp_C] [qw temp_W] 
  --Tw(0) cannot be presented, there is one more constraint Tw(0) = Tinit
  [TCon AssumedCon $ 0 $< C time $< C time_final] []

eBalanceOnWtr :: RelationConcept
eBalanceOnWtr = makeRC "eBalanceOnWtr" (nounPhraseSP $ "Energy balance on " ++
  "water to find the temperature of the water") balWtrDesc balWtr_Rel

balWtr_Rel :: Relation
balWtr_Rel = (Deriv Total (C temp_W) time) $= (Int 1) / (C tau_W) *
  (((C temp_C) - (FCall (C temp_W) [C time])))

balWtrDesc :: Sentence
balWtrDesc = foldlSent [(E $ C temp_W) `isThe` phrase temp_W +:+.
  sParen (unwrap $ getUnit temp_W), 
  (E $ C temp_C) `isThe` phrase temp_C +:+. sParen (unwrap $ getUnit temp_C),
  (E $ C tau_W $= (C w_mass * C htCap_W) / (C coil_HTC * C coil_SA)),
  S "is a constant" +:+. sParen (unwrap $ getUnit tau_W),
  S "The above", phrase equation, S "applies as long as the", phrase water,
  S "is in", phrase liquid, S "form" `sC` (E $ Int 0 $< C temp_W $< (Int 100)),
  sParen (unwrap $ getUnit temp_W), S "where", S $ show (0 :: Integer),
  sParen (unwrap $ getUnit temp_W) `sAnd` (S $ show (100 :: Integer)),
  sParen (unwrap $ getUnit temp_W), S "are the", phrase melting `sAnd`
  plural boil_pt, S "of", phrase water `sC` S "respectively",
  sParen (makeRef (mkAssump "assump10" EmptyS))]
  
