-- | Defines concepts used in the field of solid mechanics.
module Data.Drasil.Concepts.SolidMechanics where

import Drasil.Database (mkUid)
import Language.Drasil
import Data.Drasil.Concepts.Physics (force)

--FIXME: add "shear stress" and "shear strain" when we have adjectives
--       to make a combined "mobilized shear force" for example
-- | Collects all solid mechanics-related concepts.
solidcon :: [ConceptChunk]
solidcon = [mobShear, normForce, shearForce, shearRes]

mobShear, normForce, shearForce, shearRes :: ConceptChunk

mobShear   = cncpt''' (mkUid "mobShear") (cn "mobilized shear force")
  (S "the" +:+ phrase shearForce +:+ S "in the direction of potential motion" `sC`
  S "thus encouraging motion along the plane")

normForce  = cncpt''' (mkUid "normForce") (cn' "normal force")
  (S "a" +:+ phrase force +:+ S "applied perpendicular to the plane of the material")

shearRes   = cncpt''' (mkUid "shearRes") (cn "resistive shear force")
  (S "the" +:+ phrase shearForce +:+ S "in the direction opposite to the direction" +:+
  S "of potential motion, thus hindering motion along the plane")

shearForce = cncpt''' (mkUid "shearForce") (cn' "shear force")
  (S "a" +:+ phrase force +:+ S "applied parallel to the plane of the material")
