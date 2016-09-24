module Drasil.GlassBR.GlassBRUnits where

import Control.Lens ((^.))
import Language.Drasil
import Data.Drasil.SI_Units

--N^(-7)*m^12--
sFlawPU :: DerUChunk
sFlawPU = makeDerU (unitCon "surface flaw parameter") sFlawPUeqn

sFlawPUeqn :: UDefn
sFlawPUeqn = USynonym (UProd [(UPow (newton ^. unit) (-7)),
  (UPow (metre ^. unit) (12))])
