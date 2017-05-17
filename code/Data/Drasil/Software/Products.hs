module Data.Drasil.Software.Products where
import Language.Drasil
import Data.Drasil.Concepts.Documentation

matlab :: NPNC

matlab          = npnc' "matlab"        (pn' "MATLAB programming language")       "MATLAB" 


videoGame, openSource :: NPNC

videoGame                    = compoundNPNC video game
openSource                   = compoundNPNC open source