module Data.Drasil.Quantities.Physics where

import Language.Drasil
import Data.Drasil.Concepts.Physics as CP

surface :: VarChunk
surface = vcFromCC CP.surface cS --Maybe should be physical property?

restitutionCoef :: VarChunk
restitutionCoef = vcFromCC CP.restitutionCoef (sub cC cR)
