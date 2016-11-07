module Data.Drasil.Quantities.PhysicalProperties where

import Language.Drasil
import Data.Drasil.Concepts.PhysicalProperties as CPP

mass :: VarChunk
mass = vcFromCC CPP.mass lM