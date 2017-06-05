module Data.Drasil.Software.Products where
import Language.Drasil
import Data.Drasil.Concepts.Documentation

matlab, sciCompS :: NamedChunk

matlab     = npnc' "matlab"        (pn' "MATLAB programming language")       "MATLAB" 
sciCompS   = npnc "sciCompS"       (cn' "scientific computing software")

videoGame, openSource :: NamedChunk

videoGame                    = compoundNC video game
openSource                   = compoundNC open source