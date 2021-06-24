{-# Language TemplateHaskell #-}
module Language.Drasil.Reference (Reference(Reference, refInfo), ref, refS,
  namedRef, complexRef, namedComplexRef) where

import Language.Drasil.Classes.Core (HasUID(uid), HasRefAddress(getRefAdd),
  Referable(refAdd, renderRef))
import Language.Drasil.Classes.Core2 (HasShortName(shortname))
import Language.Drasil.RefProg (RefInfo(..))
import Language.Drasil.Sentence (Sentence(Ref, EmptyS))
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
-- | Finds the reference information of a 'Reference'.
instance Referable Reference where
  refAdd r = r ^. ui
  renderRef = ra

-------------------------------

-- | Projector function that creates a 'Reference' from something 'Referable'.
ref :: (Referable r, HasShortName r) => r -> Reference
ref r = Reference (r ^. uid) (renderRef r) (shortname r) None

-- Maybe just use r ^. uid without 'ref'?
-- | Takes the reference 'UID' and wraps it into a 'Sentence'.
refS :: (Referable r, HasShortName r) => r -> Sentence
refS r = namedRef r EmptyS

-- | Takes a 'Reference' with a name to be displayed and wraps it into a 'Sentence'.
-- Does not overwrite the shortname contained in the reference, but will only display as the given 'Sentence'.
namedRef :: (Referable r, HasShortName r) => r -> Sentence -> Sentence
namedRef r s = namedComplexRef r s None

complexRef :: (Referable r, HasShortName r) => r -> RefInfo -> Sentence
complexRef r = Ref (ref r ^. uid) EmptyS

-- | Takes a 'Reference' with a name to be displayed and any additional information and wraps it into a 'Sentence'.
-- Does not overwrite the shortname contained in the reference, but will only display as the given 'Sentence' along with the given 'RefInfo'.
namedComplexRef :: (Referable r, HasShortName r) => r -> Sentence -> RefInfo -> Sentence
namedComplexRef r = Ref (ref r ^. uid)