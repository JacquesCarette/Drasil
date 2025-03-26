{-# Language TemplateHaskell #-}
-- | Defines a type used to hold referencing information.
module Language.Drasil.Reference (
  -- * Type
  Reference(Reference),
  -- * Class
  HasReference(..),
  -- * Constructors
  ref, refS, namedRef, complexRef, namedComplexRef
) where

import Control.Lens ((^.), makeLenses, Lens')

import Drasil.Database.UID (UID, HasUID(..))
import Drasil.Language.ShortName (HasShortName(..), ShortName)

import Language.Drasil.Label.Type (LblType, HasRefAddress(..))
import Language.Drasil.Sentence (Sentence(Ref, EmptyS), RefInfo(..))


-- | A Reference contains the identifier ('UID'), a reference address ('LblType'),
-- a human-readable shortname ('ShortName'), and any extra information about the reference ('RefInfo').
data Reference = Reference
  { _ui :: UID
  ,  ra :: LblType
  ,  sn :: ShortName}
makeLenses ''Reference

-- | A class that contains a list of 'Reference's.
class HasReference c where
  -- | Provides a 'Lens' to the 'Reference's.
  getReferences :: Lens' c [Reference]

-- | Equal if 'UID's are equal.
instance Eq            Reference where a == b = (a ^. uid) == (b ^. uid)
-- | Finds the 'UID' of a 'Reference'.
instance HasUID        Reference where uid = ui
-- | Finds the reference address contained in a 'Reference' (through a 'LblType').
instance HasRefAddress Reference where getRefAdd = ra
-- | Finds the shortname of the reference address used for the 'Reference'.
instance HasShortName  Reference where shortname = sn
{--- Finds the reference information of a 'Reference'.
instance Referable Reference where
  refAdd r = r ^. ui
  renderRef = ra-}

-------------------------------

-- | Projector function that creates a 'Reference' from something 'Referable'.
ref :: (HasUID r, HasRefAddress r, HasShortName r) => r -> Reference
ref r = Reference (r ^. uid) (getRefAdd r) (shortname r)

-- Maybe just use r ^. uid without 'ref'?
-- | Takes the reference 'UID' and wraps it into a 'Sentence'.
refS :: (HasUID r, HasRefAddress r, HasShortName r) => r -> Sentence
refS r = namedRef r EmptyS

-- | Takes a 'Reference' with a name to be displayed and wraps it into a 'Sentence'.
-- Does not overwrite the shortname contained in the reference, but will only display as the given 'Sentence'.
namedRef :: (HasUID r, HasRefAddress r, HasShortName r) => r -> Sentence -> Sentence
namedRef r s = namedComplexRef r s None

-- | Takes a 'Reference' with additional display info. Uses the internal shortname for its display name.
complexRef :: (HasUID r, HasRefAddress r, HasShortName r) => r -> RefInfo -> Sentence
complexRef r = Ref (ref r ^. uid) EmptyS

-- | Takes a 'Reference' with a name to be displayed and any additional information and wraps it into a 'Sentence'.
-- Does not overwrite the shortname contained in the reference, but will only display as the given 'Sentence' along with the given 'RefInfo'.
namedComplexRef :: (HasUID r, HasRefAddress r, HasShortName r) => r -> Sentence -> RefInfo -> Sentence
namedComplexRef r = Ref (ref r ^. uid)