module Data.Drasil.Concepts.SolidMechanics where

import Language.Drasil
import Data.Drasil.Concepts.Physics (force, friction, strain, stress)

--FIXME: add "shear stress" and "shear strain" when we have adjectives
--       to make a combined "mobilized shear force" for example
solidcon :: [ConceptChunk]
solidcon = [elastMod, mobShear, normForce, nrmStrss, poissnsR, shearForce,
  shearRes, stffness]

elastMod, mobShear, normForce, nrmStrss, poissnsR, shearForce,
  shearRes, stffness :: ConceptChunk

elastMod   = dccWDS "elastMod" (cn "elastic modulus") 
  (S "The ratio of the" +:+ phrase stress +:+ 
  S "exerted on a body to the resulting" +:+. phrase strain)

mobShear   = dccWDS "mobShear" (cn "mobilized shear force") 
  (at_start shearForce +:+ S "in the direction of potential motion" `sC`
  S "thus encouraging motion along the plane.")

normForce  = dccWDS "normForce" (cn' "normal force")
  (S "A" +:+ phrase force +:+ S "applied perpendicular" +:+ 
  S "to the plane of the material.")
  
nrmStrss   = dccWDS "nrmStrss" (cn "normal stress") 
  (S "The" +:+ phrase stress +:+ 
  S "exerted perpendicular to the plane of the object")
  
poissnsR   = dccWDS "poissnsR" (nounPhraseSP "Poisson's ratio") 
  (S "The ratio of perpendicular" +:+ phrase strain +:+ 
  S "to parallel" +:+. phrase strain)
  
shearRes   = dccWDS "shearRes" (cn "resistive shear force") 
  (at_start shearForce +:+ S "in the direction opposite to the direction" +:+
  S "of potential motion, thus hindering motion along the plane.")
  
shearForce = dccWDS "shearForce" (cn' "shear force")
  (S "A" +:+ phrase force +:+ S "applied parallel to the plane of the material.")
  
stffness   = dccWDS "stffness" (cn "stiffness") 
  (S "The extent a body" +:+ S "resists" +:+. phrase strain)
