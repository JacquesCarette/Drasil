module Data.Drasil.Software.Products where
import Language.Drasil (CI, NamedChunk, cn', commonIdea, compoundNC, nc, pn')
import Data.Drasil.Concepts.Documentation (game, video, open, source)
import Data.Drasil.Concepts.Computation (computer)
import Data.Drasil.Concepts.Software (program)

matlab :: CI
matlab     = commonIdea "matlab" (pn' "MATLAB programming language")       "MATLAB" 

sciCompS :: NamedChunk
sciCompS   = nc "sciCompS"       (cn' "scientific computing software")

videoGame, openSource, compPro :: NamedChunk
videoGame   = compoundNC video game
openSource  = compoundNC open source
compPro     = compoundNC computer program
