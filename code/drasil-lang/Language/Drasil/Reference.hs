module Language.Drasil.Reference where

import Control.Lens ((^.))

import Language.Drasil.Chunk.Citation (Citation)
import Language.Drasil.Classes.Core (HasUID(uid), HasShortName(shortname))
import Language.Drasil.Classes (Referable(renderRef))
import Language.Drasil.RefProg (Reference(..), RefInfo(..), InfoType)
import Language.Drasil.Sentence (Sentence(Ref))

makeRef2 :: (Referable l, HasShortName l) => l -> Reference
makeRef2 l = Reference (l ^. uid) (renderRef l) (shortname l) None

makeRef2S :: (Referable l, HasShortName l) => l -> Sentence
makeRef2S = Ref . makeRef2

-- Here we don't use the Lenses as constraints, we really do want a Citation.
makeCite :: Citation -> Reference
makeCite l = Reference (l ^. uid) (renderRef l) (shortname l) None

makeCiteS :: Citation -> Sentence
makeCiteS = Ref . makeCite

-- Makes a Reference from a Citation with additional information
makeCiteInfo :: Citation -> InfoType -> [Int] -> Reference
makeCiteInfo _ _ [ ] = error "List of integers is empty for makeCiteInfo"
makeCiteInfo l i lst = Reference (l ^. uid) (renderRef l) (shortname l) (RI i lst)

makeCiteInfoS :: Citation -> InfoType -> [Int] -> Sentence
makeCiteInfoS c i lst = Ref $ makeCiteInfo c i lst
