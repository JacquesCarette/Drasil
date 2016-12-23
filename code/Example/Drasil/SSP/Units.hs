module Drasil.SSP.Units where

import Language.Drasil
import Data.Drasil.SI_Units

import Control.Lens ((^.))

--degree--

-- FIXME: Pull this out.

degree :: FundUnit
degree = UD (dcc "degree" "Degree" "angle") (UName (Special Circle))

--unitless--

-- FIXME: Remove this.
unitless :: FundUnit
unitless = UD (dcc "unitless" "Unitless" "unitless") (UName $ Atomic "unitless")

--N/m^3--
specific_weight :: DerUChunk
specific_weight = makeDerU (dcc "specific_weight" "specific weight" 
  "weight per unit volume") $
  USynonym (UDiv (newton ^. unit) (UPow (metre ^. unit) (3)))