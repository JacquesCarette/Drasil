module Language.Drasil.Reference(makeRef2, makeRef2S, makeCite, makeCiteS) where

import Control.Lens ((^.))

import Language.Drasil.Chunk.Citation (Citation)
import Language.Drasil.Classes.Core (HasUID(uid), HasShortName(shortname))
import Language.Drasil.Classes (Referable(renderRef))
import Language.Drasil.RefProg (Reference(Reference))
import Language.Drasil.Sentence (Sentence(Ref))

makeRef2 :: (Referable l, HasShortName l) => l -> Reference
makeRef2 l = Reference (l ^. uid) (renderRef l) (shortname l)

makeRef2S :: (Referable l, HasShortName l) => l -> Sentence
makeRef2S = Ref . makeRef2

-- Here we don't use the Lenses as constraints, we really do want a Citation.
makeCite :: Citation -> Reference
makeCite l = Reference (l ^. uid) (renderRef l) (shortname l)

makeCiteS :: Citation -> Sentence
makeCiteS = Ref . makeCite

