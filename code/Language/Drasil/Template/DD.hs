module Language.Drasil.Template.DD (makeDD) where

import Language.Drasil.Document
import Language.Drasil.Chunk.Module
import Language.Drasil.Template.MG
import Language.Drasil.Template.MIS
import Language.Drasil.Template.Helpers

makeDD :: [ModuleChunk] -> ([Section], [Section])
makeDD mcs = let (mg, modules) = makeMG mcs
                 mis           = makeMIS $ getMISModules modules
             in  (mg, mis)