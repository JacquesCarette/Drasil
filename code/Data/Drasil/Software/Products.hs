module Data.Drasil.Software.Products where
import Language.Drasil


chipmunk, matlab :: NPNC

matlab          = npnc' "matlab"        (pn' "MATLAB programming language")       "MATLAB"
chipmunk        = npnc' "chipmunk"      (pn "Chipmunk2D game physics library")    "Chipmunk2D" 
