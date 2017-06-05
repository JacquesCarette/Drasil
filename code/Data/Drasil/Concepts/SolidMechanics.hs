module Data.Drasil.Concepts.SolidMechanics where

import Data.Drasil.Concepts.Physics
import Language.Drasil
import Control.Lens((^.))

--FIXME: add "shear stress" and "shear strain" when we have adjectives
--       to make a combined "mobilized shear force" for example

elastMod, mobShear, normForce, poissnsR, shearForce,
  shearRes, stffness :: ConceptChunk

elastMod   = dccWDS "E" (cn "elastic modulus") (S "The ratio of the" +:+
  (phrase stress) +:+ S "exerted on a body to the resulting" 
  +:+. (phrase strain))

mobShear   = dccWDS "S" (cn "mobilized shear force") (S "The amount of" +:+
  S "shear" +:+ (phrase force) +:+ S "resisted by the body" +:+
  S "when shear" +:+ (phrase stress) +:+ S "is applied.")

normForce  = dccWDS "normForce" (cn' "normal force")
  (S "A" +:+ (phrase force) +:+ S "applied perpendicular" +:+ 
  S "to the plane of the material.")
  
poissnsR   = dccWDS "nu" (nounPhraseSP "Poisson's ratio") (S "The ratio" +:+
  S "of perpendicular" +:+ (phrase strain) +:+ S "to parellel" +:+.
  (phrase strain))
  
shearRes   = dccWDS "P" (cn "shear resistance") (S "The resulting" +:+
  (phrase friction) +:+ S "caused by a shear" +:+. 
  (phrase stress))
  
shearForce = dccWDS "shearForce" (cn' "shear force")
  (S "A" +:+ (phrase force) +:+ S "applied parallel" +:+ 
  S "to the plane of the material.")
  
stffness   = dccWDS "K" (cn "stiffness") (S "The extent a body" +:+
  S "resists" +:+. (phrase strain))