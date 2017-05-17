module Data.Drasil.Concepts.SolidMechanics where

import Data.Drasil.Concepts.Physics
import Language.Drasil
import Control.Lens((^.))

--FIXME: add "shear stress" and "shear strain" when we have adjectives
--       to make a combined "mobilized shear force" for example

elastMod, mobShear, normForce, poissnsR, shearForce,
  shearRes, stffness :: ConceptChunk

elastMod   = dccWDS "E" (cn "elastic modulus") (S "The ratio of the" +:+
  (phrase $ stress ^. term) +:+ S "exerted on a body to the resulting" 
  +:+. (phrase $ strain ^. term))

mobShear   = dccWDS "S" (cn "mobilized shear force") (S "The amount of" +:+
  S "shear" +:+ (phrase $ force ^. term) +:+ S "resisted by the body" +:+
  S "when shear" +:+ (phrase $ stress ^. term) +:+ S "is applied.")

normForce  = dccWDS "normForce" (cn' "normal force")
  (S "A" +:+ (phrase $ force ^. term) +:+ S "applied perpendicular" +:+ 
  S "to the plane of the material.")
  
poissnsR   = dccWDS "nu" (nounPhraseSP "Poisson's ratio") (S "The ratio" +:+
  S "of perpendicular" +:+ (phrase $ strain ^. term) +:+ S "to parellel" +:+.
  (phrase $ strain ^. term))
  
shearRes   = dccWDS "P" (cn "shear resistance") (S "The resulting" +:+
  (phrase $ friction ^. term) +:+ S "caused by a shear" +:+. 
  (phrase $ stress ^. term))
  
shearForce = dccWDS "shearForce" (cn' "shear force")
  (S "A" +:+ (phrase $ force ^. term) +:+ S "applied parallel" +:+ 
  S "to the plane of the material.")
  
stffness   = dccWDS "K" (cn "stiffness") (S "The extent a body" +:+
  S "resists" +:+. (phrase $ strain ^. term))