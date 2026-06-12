-- | Defines concepts used in the field of solid mechanics.
module Data.Drasil.Concepts.SolidMechanics where

import Language.Drasil
import Data.Drasil.Concepts.Physics (force, strain, stress)

--FIXME: add "shear stress" and "shear strain" when we have adjectives
--       to make a combined "mobilized shear force" for example
-- | Collects all solid mechanics-related concepts.
solidcon :: [ConceptChunk]
solidcon = [elastMod, mobShear, normForce, nrmStrss, poissnsR, shearForce,
  shearRes, stffness]

elastMod, mobShear, normForce, nrmStrss, poissnsR, shearForce,
  shearRes, stffness :: ConceptChunk

elastMod   = cncpt'' "elastMod" (cn "elastic modulus")
  (S "the ratio of the" +:+ phrase stress +:+
  S "exerted on a body to the resulting" +:+ phrase strain)

mobShear   = cncpt'' "mobShear" (cn "mobilized shear force")
  (S "the" +:+ phrase shearForce +:+ S "in the direction of potential motion" `sC`
  S "thus encouraging motion along the plane")

normForce  = cncpt'' "normForce" (cn' "normal force")
  (S "a" +:+ phrase force +:+ S "applied perpendicular to the plane of the material")

nrmStrss   = cncpt'' "nrmStrss" (cn "normal stress")
  (S "the" +:+ phrase stress +:+  S "exerted perpendicular to the plane of the object")

poissnsR   = cncpt'' "poissnsR" (nounPhraseSP "Poisson's ratio")
  (S "the ratio of perpendicular" +:+ phrase strain +:+ S "to parallel" +:+ phrase strain)

shearRes   = cncpt'' "shearRes" (cn "resistive shear force")
  (S "the" +:+ phrase shearForce +:+ S "in the direction opposite to the direction" +:+
  S "of potential motion, thus hindering motion along the plane")

shearForce = cncpt'' "shearForce" (cn' "shear force")
  (S "a" +:+ phrase force +:+ S "applied parallel to the plane of the material")

stffness   = cncpt'' "stffness" (cn "stiffness")
  (S "the extent a body resists" +:+ phrase strain)
