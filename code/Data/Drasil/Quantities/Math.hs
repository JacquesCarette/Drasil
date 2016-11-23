module Data.Drasil.Quantities.Math where

import Language.Drasil
import Data.Drasil.Concepts.Math as CM

gradient :: DefinedQuantity
norm_vect :: VarChunk

gradient  = dqFromDCC CM.gradient (Greek Nabla)
norm_vect = vcFromCC CM.norm_vect (vec $ hat lN)