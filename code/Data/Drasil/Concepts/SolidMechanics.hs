module Data.Drasil.Concepts.SolidMechanics where

import Language.Drasil
import Data.Drasil.Concepts.Physics (force, friction, strain, stress)

--FIXME: add "shear stress" and "shear strain" when we have adjectives
--       to make a combined "mobilized shear force" for example

elastMod, mobShear, normForce, nrmStrss, poissnsR, shearForce,
  shearRes, stffness :: ConceptChunk

elastMod   = dccWDS "elastMod" (cn "elastic modulus") 
  (S "The ratio of the" +:+ phrase stress +:+ 
  S "exerted on a body to the resulting" +:+. phrase strain)

mobShear   = dccWDS "mobShear" (cn "mobilized shear force") 
  (S "The amount of shear" +:+ (phrase force) +:+ S "resisted by the body" +:+
  S "when shear" +:+ (phrase stress) +:+ S "is applied.")

normForce  = dccWDS "normForce" (cn' "normal force")
  (S "A" +:+ phrase force +:+ S "applied perpendicular" +:+ 
  S "to the plane of the material.")
  
nrmStrss   = dccWDS "nrmStrss" (cn "normal stress") 
  (S "The" +:+ phrase stress +:+ 
  S "exerted perpendicular to the plane of the object")
  
poissnsR   = dccWDS "poissnsR" (nounPhraseSP "Poisson's ratio") 
  (S "The ratio of perpendicular" +:+ phrase strain +:+ 
  S "to parallel" +:+. phrase strain)
  
shearRes   = dccWDS "shearRes" (cn "shear resistance") 
  (S "The resulting" +:+ phrase friction +:+ 
  S "caused by a shear" +:+. phrase stress)
  
shearForce = dccWDS "shearForce" (cn' "shear force")
  (S "A" +:+ phrase force +:+ S "applied parallel to the plane of the material.")
  
stffness   = dccWDS "stffness" (cn "stiffness") 
  (S "The extent a body" +:+ S "resists" +:+. phrase strain)
