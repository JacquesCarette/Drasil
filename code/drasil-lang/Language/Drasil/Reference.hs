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
-- The following functions have the same functions as makeRef2, makeCite, and makeCiteInfo respectively. --

-- | Smart constructor for making a 'Referable' 'Reference'.
rw :: (Referable r, HasShortName r) => r -> Reference
rw r = Reference (r ^. uid) (renderRef r) (shortname r) None -- None is here as all additional information is treated as display info in a 'Sentence'.

-- Lower level version of rw (only needs a ref address, not to be Referable). Is this even needed?
-- Smart constructor for making a 'Reference'. Does not need to be 'Referable'.
-- Defers 'LblType' lookup. Uses 'RP' to construct 'LblType'.
-- Citations should not be mapped with this, should instead use rw'.
--rw :: (HasUID r, HasRefAddress r, HasShortName r) => r -> Reference
--rw r = Reference (r ^. uid) (RP (defer (r ^. uid)) (getRefAdd r)) (shortname r) None -- None and the defer bit may be a hack

-- | Smart constructor for converting a 'Citation' into a 'Reference'. Does not take in additional information.
rwCite :: Citation -> Reference
rwCite r = Reference (r ^. uid) (renderRef r) (shortname r) None

{--- This is probably a hack. Because we are deferring reference lookups
-- instead of rendering them on the spot, they may need different UIDs to hold different info.
-- For example, the RefInfo of two references may be different but one will overwrite the other if they have the same UID.
-- | Smart constructor for converting a 'Citation' into a 'Reference'. Does not take in additional information.
rwCiteInfo :: Citation -> RefInfo -> Reference
rwCiteInfo r None = rwCite r
rwCiteInfo r (Equation xs) = Reference (r ^. uid ++ "Eq" ++ concatMap show xs) (renderRef r) (shortname r) (Equation xs)
rwCiteInfo r (Page xs) = Reference (r ^. uid ++ "Page" ++ concatMap show xs) (renderRef r) (shortname r) (Page xs)
rwCiteInfo r (RefNote n) = Reference (r ^. uid ++ n) (renderRef r) (shortname r) (RefNote n)-}