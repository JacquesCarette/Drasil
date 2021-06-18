{-# Language TemplateHaskell #-}
module Language.Drasil.Reference (Reference(Reference, refInfo), makeRef2, makeRef2S,
  makeCite, makeCiteS, makeCiteInfo, makeCiteInfoS, rw) where

import Language.Drasil.Chunk.Citation (Citation)
import Language.Drasil.Classes.Core (HasUID(uid), HasRefAddress(getRefAdd),
  Referable(refAdd, renderRef))
import Language.Drasil.Classes.Core2 (HasShortName(shortname))
import Language.Drasil.RefProg (RefInfo(..))
import Language.Drasil.Sentence (Sentence(Ref))
import Language.Drasil.Label.Type (LblType(..), getAdd)
import Language.Drasil.ShortName (ShortName)
import Language.Drasil.UID (UID)

import Control.Lens ((^.), makeLenses)

--Reference Type--

-- | A Reference contains the identifier ('UID'), a reference address ('LblType'),
-- a human-readable shortname ('ShortName'), and any extra information about the reference ('RefInfo').
data Reference = Reference
  { _ui :: UID
  ,  ra :: LblType
  ,  sn :: ShortName
  ,  refInfo :: RefInfo }
makeLenses ''Reference

-- | Equal if 'UID's are equal.
instance Eq            Reference where a == b = (a ^. uid) == (b ^. uid)
-- | Finds the 'UID' of a 'Reference'.
instance HasUID        Reference where uid = ui
-- | Finds the reference address contained in a 'Reference' (through a 'LblType').
instance HasRefAddress Reference where getRefAdd = getAdd . ra
-- | Finds the shortname of the reference address used for the 'Reference'.
instance HasShortName  Reference where shortname = sn
-- | Finds the reference address of a 'Reference'.
instance Referable Reference where
  refAdd r = r ^. ui
  renderRef = ra

-------------------------------

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

-- | Makes a 'Reference' from a 'Citation' with additional information.
makeCiteInfo :: Citation -> RefInfo -> Reference
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
