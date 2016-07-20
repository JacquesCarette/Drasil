{-# OPTIONS -Wall #-}
module Example.Drasil.GlassBR.GlassBRUnits where

import Example.Drasil.GlassBR.GlassBRSIUnits
import Control.Lens ((^.))
import Language.Drasil

--N^(-7)*m^12--
sFlawPU :: DerUChunk
sFlawPU = makeDerU (unitCon "surface flaw parameter") sFlawPUeqn

sFlawPUeqn :: UDefn
sFlawPUeqn = USynonym (UProd [(UPow (newton ^. unit) (-7)),
  (UPow (metre ^. unit) (12))])
