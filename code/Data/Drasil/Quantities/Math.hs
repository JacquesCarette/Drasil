module Data.Drasil.Quantities.Math where

import Language.Drasil
import Data.Drasil.Concepts.Math as CM

gradient = vcFromCC CM.gradient (Greek Nabla)