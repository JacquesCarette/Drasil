module Language.Drasil.Reference where

import Control.Lens ((^.))

import Language.Drasil.Chunk.Citation (Citation)
import Language.Drasil.Classes.Core (HasUID(uid), HasShortName(shortname), Referable(renderRef))
import Language.Drasil.RefProg (Reference(..), RefInfo(..))
import Language.Drasil.Sentence (Sentence(Ref))

-- | Projector function that creates a 'Reference' from something 'Referable'.
makeRef2 :: (Referable l, HasShortName l) => l -> Reference
makeRef2 l = Reference (l ^. uid) (renderRef l) (shortname l) None

-- Maybe just use l ^. uid without makeRef2?
-- | Takes the reference 'UID' and wraps it into a 'Sentence'.
makeRef2S :: (Referable l, HasShortName l) => l -> Sentence
makeRef2S l = Ref (makeRef2 l ^. uid) None

-- Here we don't use the Lenses as constraints, we really do want a Citation.
-- | Similar to `makeRef2`, but only turns a citation into a reference.
makeCite :: Citation -> Reference
makeCite l = Reference (l ^. uid) (renderRef l) (shortname l) None

-- | Similar to `makeRef2S`, but only takes a citation.
makeCiteS :: Citation -> Sentence
makeCiteS l = Ref (makeCite l ^. uid) None

-- Changing UIDs may be a hack.
-- | Makes a 'Reference' from a 'Citation' with additional information.
makeCiteInfo :: Citation -> RefInfo -> Reference
{-makeCiteInfo l None = Reference (l ^. uid) (renderRef l) (shortname l) None
makeCiteInfo l (Equation xs) = Reference (l ^. uid ++ "Eq" ++ concatMap show xs) (renderRef l) (shortname l) (Equation xs)
makeCiteInfo l (Page xs) = Reference (l ^. uid ++ "Page" ++ concatMap show xs) (renderRef l) (shortname l) (Page xs)
makeCiteInfo l (RefNote n) = Reference (l ^. uid) (renderRef l) (shortname l) (RefNote n)-}
makeCiteInfo l = Reference (l ^. uid) (renderRef l) (shortname l)

-- | Makes a 'Reference' from a 'Citation' with additional information
-- and then wraps into 'Sentence' form.
makeCiteInfoS :: Citation -> RefInfo -> Sentence
makeCiteInfoS c ri = Ref (makeCiteInfo c ri ^. uid) ri

---------------------------------------
-- The following function is the same as makeRef2, but renamed for clarity. --

-- | Smart constructor for making a 'Referable' 'Reference'.
rw :: (Referable r, HasShortName r) => r -> Reference
rw r = Reference (r ^. uid) (renderRef r) (shortname r) None -- None is here as all additional information is treated as display info in a 'Sentence'.
