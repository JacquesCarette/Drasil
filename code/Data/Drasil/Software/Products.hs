module Data.Drasil.Software.Products where
import Language.Drasil
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.Computation (computer)
import Data.Drasil.Concepts.Software (program)

matlab :: CI
sciCompS :: NamedChunk

matlab     = commonIdea "matlab" (pn' "MATLAB programming language")       "MATLAB" 
sciCompS   = nc "sciCompS"       (cn' "scientific computing software")

videoGame, openSource, compPro :: NamedChunk

videoGame                    = compoundNC video game
openSource                   = compoundNC open source
compPro                      = compoundNC computer program
