module Data.Drasil.Quantities.SolidMechanics where

import Language.Drasil
import Data.Drasil.Concepts.SolidMechanics as CSM (elastMod, mobShear, nrmStrss,
    poissnsR, shearRes, stffness)
import Data.Drasil.SI_Units (newton, pascal)
import Data.Drasil.Units.SolidMechanics (stiffnessU)

elastMod, mobShear, nrmStrss, shearRes, stffness :: UnitalChunk

elastMod = uc CSM.elastMod cE pascal 
mobShear = uc CSM.mobShear cS newton 
shearRes = uc CSM.shearRes cP newton 
stffness = uc CSM.stffness cK stiffnessU 
nrmStrss = uc CSM.nrmStrss (Greek Sigma_L) pascal

poissnsR :: DefinedQuantityDict
poissnsR = dqd' CSM.poissnsR (const $ Greek Nu_L) Real Nothing 
