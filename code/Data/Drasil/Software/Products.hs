module Data.Drasil.Software.Products where
import Language.Drasil
import Data.Drasil.Concepts.Documentation

chipmunk, matlab :: NPNC

matlab          = npnc' "matlab"        (pn' "MATLAB programming language")       "MATLAB"
chipmunk        = npnc' "chipmunk"      (pn "Chipmunk2D game physics library")    "Chipmunk2D" 




videoGame, openSource :: NPNC

videoGame                    = compoundNPNC video game
openSource                   = compoundNPNC open source