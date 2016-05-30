{-# OPTIONS -Wall #-}
module GlassBRUnits1 where

import Language.Drasil.SI_Units2
import Language.Drasil.Unit (Unit(..), UDefn(..), DerUChunk(..),
  makeDerU, unitCon)
import Language.Drasil.Chunk ()
import Control.Lens ((^.))
import Language.Drasil.Spec (USymb(..))

--N^(-7)*m^12--
sFlawPU :: DerUChunk
sFlawPU = makeDerU (unitCon "surface flaw parameter") sFlawPUeqn

sFlawPUeqn :: UDefn
sFlawPUeqn = USynonym (UProd [(UPow (newton ^. unit) (-7)),
  (UPow (metre ^. unit) (12))])
