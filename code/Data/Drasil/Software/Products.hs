module Data.Drasil.Software.Products where
import Language.Drasil
import Data.Drasil.Concepts.Documentation

matlab, sciCompS :: NPNC

matlab     = npnc' "matlab"        (pn' "MATLAB programming language")       "MATLAB" 
sciCompS   = npnc "sciCompS"       (cn' "scientific computing software")

videoGame, openSource :: NPNC

videoGame                    = compoundNPNC video game
openSource                   = compoundNPNC open source