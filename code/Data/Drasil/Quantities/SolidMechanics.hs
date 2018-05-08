module Data.Drasil.Quantities.SolidMechanics where

import Language.Drasil
import Data.Drasil.Concepts.SolidMechanics as CSM
import Data.Drasil.Units.SolidMechanics
import Data.Drasil.SI_Units

elastMod, mobShear, nrmStrss, shearRes, stffness :: UnitalChunk

elastMod = uc CSM.elastMod cE pascal
mobShear = uc CSM.mobShear cS newton
shearRes = uc CSM.shearRes cP newton
stffness = uc CSM.stffness cK stiffnessU
nrmStrss = uc CSM.nrmStrss (Greek Sigma_L) pascal

poissnsR :: ConVar
poissnsR = cv CSM.poissnsR (lNu) Real
