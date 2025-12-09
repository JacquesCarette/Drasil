-- | Assigns symbols and units (quantities) to mechanics-related concepts.
module Data.Drasil.Quantities.SolidMechanics where

import Language.Drasil
import Language.Drasil.ShortHands (cE, cS, cP, cK, lSigma, lNu)

import Data.Drasil.Concepts.SolidMechanics as CSM (elastMod, mobShear, nrmStrss,
    poissnsR, shearRes, stffness)
import Data.Drasil.SI_Units (newton, pascal)
import Data.Drasil.Units.SolidMechanics (stiffnessU)

-- * With Units

elastMod, mobShear, nrmStrss, shearRes, stffness :: UnitalChunk

elastMod = uc CSM.elastMod cE     Real pascal
mobShear = uc CSM.mobShear cS     Real newton
shearRes = uc CSM.shearRes cP     Real newton
stffness = uc CSM.stffness cK     Real stiffnessU
nrmStrss = uc CSM.nrmStrss lSigma Real pascal

-- * Without Units

poissnsR :: DefinedQuantityDict
poissnsR = dqdNoUnit CSM.poissnsR lNu Real
