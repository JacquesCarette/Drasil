module Data.Drasil.Quantities.Math where

import Language.Drasil
import Data.Drasil.Concepts.Math as CM

gradient, norm_vect :: ConVar

gradient  = cvR CM.gradient (Greek Nabla)
norm_vect = cvR CM.norm_vect (vec $ hat lN)