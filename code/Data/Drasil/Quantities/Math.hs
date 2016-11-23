module Data.Drasil.Quantities.Math where

import Language.Drasil
import Data.Drasil.Concepts.Math as CM

gradient, norm_vect :: DefinedQuantity

gradient  = dqFromDCC CM.gradient (Greek Nabla)
norm_vect = dqFromDCC CM.norm_vect (vec $ hat lN)