module Data.Drasil.Quantities.SolidMechanics where

import Language.Drasil
import Data.Drasil.Concepts.SolidMechanics as CSM
import Data.Drasil.Units.SolidMechanics
import Data.Drasil.SI_Units

elastMod, mobShear, nrmStrss, shearRes, stffness :: UnitalChunk

elastMod = ucEL CSM.elastMod cE pascal
mobShear = ucEL CSM.mobShear cS newton
shearRes = ucEL CSM.shearRes cP newton
stffness = ucEL CSM.stffness cK stiffnessU
nrmStrss = ucEL CSM.nrmStrss (Greek Sigma_L) pascal

poissnsR :: DefinedQuantityDict
poissnsR = cqs CSM.poissnsR (Greek Nu_L) Real
