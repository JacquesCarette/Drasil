module Language.Drasil.Template.DD (makeDD) where

import Language.Drasil.Document
import Language.Drasil.Chunk.Module
import Language.Drasil.Chunk.Other
import Language.Drasil.Chunk.Req
import Language.Drasil.Chunk.LC
import Language.Drasil.Template.MG
import Language.Drasil.Template.MIS
import Language.Drasil.Template.Helpers

makeDD :: [LCChunk] -> [UCChunk] -> [ModuleChunk] -> ([Section], [Section])
makeDD lccs uccs mcs = let (mg, modules) = makeMG lccs uccs mcs
                           mis           = makeMIS $ getMISModules modules
                       in  (mg, mis)