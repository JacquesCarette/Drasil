module Drasil.Template.DD (makeDD) where

import Language.Drasil

import Drasil.Template.MG
import Drasil.Template.MIS
import Drasil.Template.Helpers

makeDD :: [LCChunk] -> [UCChunk] -> [ReqChunk] -> [ModuleChunk]
  -> ([Section], [Section])
makeDD lccs uccs rcs mcs = let (mg, modules) = makeMG lccs uccs rcs mcs
                               mis           = makeMIS $ getMISModules modules
                           in  (mg, mis)
