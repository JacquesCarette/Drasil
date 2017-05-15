module Data.Drasil.Concepts.SolidMechanics where

import Data.Drasil.Concepts.Physics
import Language.Drasil
import Control.Lens((^.))

elastMod, poissnsR, shearRes, stffness :: ConceptChunk

elastMod = dccWDS "E" (cn "elastic modulus") (S "The ratio of the" +:+
  (phrase $ stress ^. term) +:+ S "exerted on a body to the resulting" 
  +:+. (phrase $ strain ^. term))
  
poissnsR = dccWDS "nu" (nounPhraseSP "Poisson's ratio") (S "The ratio of" +:+
  S "perpendicular" +:+ (phrase $ strain ^. term) +:+ S "to parellel" +:+.
  (phrase $ strain ^. term))
  
shearRes = dccWDS "P" (cn "shear resistance") (S "The resulting" +:+
  S "friction caused by a shear" +:+. (phrase $ stress ^. term))
  
stffness = dccWDS "K" (cn "stiffness") (S "The extent a body" +:+
  S "resists" +:+. (phrase $ strain ^. term))