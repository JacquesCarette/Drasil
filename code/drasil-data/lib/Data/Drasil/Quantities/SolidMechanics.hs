-- | Assigns symbols and units (quantities) to mechanics-related concepts.
module Data.Drasil.Quantities.SolidMechanics where

import Language.Drasil
import Language.Drasil.ShortHands (cE, cS, cP, cK, lSigma, lNu)

import Drasil.Database (mkUid)
import Data.Drasil.Concepts.SolidMechanics as CSM (elastMod, mobShear, nrmStrss,
    shearRes, stffness)
import qualified Data.Drasil.Concepts.Physics as CP (strain)
import Data.Drasil.SI_Units (newton, pascal)
import Data.Drasil.Units.SolidMechanics (stiffnessU)

-- * With Units

elastMod, mobShear, nrmStrss, shearRes, stffness :: DefinedQuantityDict

elastMod = dqd CSM.elastMod cE     Real pascal
mobShear = dqd CSM.mobShear cS     Real newton
shearRes = dqd CSM.shearRes cP     Real newton
stffness = dqd CSM.stffness cK     Real stiffnessU
nrmStrss = dqd CSM.nrmStrss lSigma Real pascal

-- * Without Units

poissnsR :: DefinedQuantityDict
poissnsR = quantNoUnit' (mkUid "poissnsR") (nounPhraseSP "Poisson's ratio")
  (S "the ratio of perpendicular" +:+ phrase CP.strain +:+ S "to parallel" +:+ phrase CP.strain)
  (const lNu) Real
