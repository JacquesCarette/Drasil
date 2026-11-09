-- | Assigns symbols and units (quantities) to mechanics-related concepts.
module Data.Drasil.Quantities.SolidMechanics where

import Language.Drasil
import Language.Drasil.ShortHands (cE, cS, cP, cK, lSigma, lNu)

import Drasil.Database (mkUid)
import Data.Drasil.Concepts.SolidMechanics as CSM (mobShear, shearRes)
import qualified Data.Drasil.Concepts.Physics as CP (strain, stress)
import Data.Drasil.SI_Units (newton, pascal)
import Data.Drasil.Units.SolidMechanics (stiffnessU)

-- * With Units

elastMod, mobShear, nrmStrss, shearRes, stffness :: DefinedQuantityDict

elastMod = quant (mkUid "elastMod") (cn "elastic modulus")
  (S "the ratio of the" +:+ phrase CP.stress +:+
  S "exerted on a body to the resulting" +:+ phrase CP.strain)
  cE Real pascal
mobShear = dqd CSM.mobShear cS     Real newton
shearRes = dqd CSM.shearRes cP     Real newton
stffness = quant (mkUid "stffness") (cn "stiffness")
  (S "the extent a body resists" +:+ phrase CP.strain)
  cK Real stiffnessU
nrmStrss = quant (mkUid "nrmStrss") (cn "normal stress")
  (S "the" +:+ phrase CP.stress +:+ S "exerted perpendicular to the plane of the object")
  lSigma Real pascal

-- * Without Units

poissnsR :: DefinedQuantityDict
poissnsR = quantNoUnit' (mkUid "poissnsR") (nounPhraseSP "Poisson's ratio")
  (S "the ratio of perpendicular" +:+ phrase CP.strain +:+ S "to parallel" +:+ phrase CP.strain)
  (const lNu) Real
