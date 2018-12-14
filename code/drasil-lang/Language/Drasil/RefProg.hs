{-# Language TemplateHaskell #-}
module Language.Drasil.RefProg 
  (RefProg(..), Reference(Reference), name, (+::+), raw, defer, prepend,
   IRefProg(..))
  where
import Language.Drasil.Classes.Core (HasUID(uid), HasRefAddress(getRefAdd),
  HasShortName(shortname))
import Language.Drasil.ShortName (ShortName)
import Language.Drasil.UID (UID)
import Language.Drasil.Label.Type (LblType)

import Control.Lens (makeLenses)

-- Trying different pieces of information for a reference
data RefProg = RP IRefProg | Citation | URI

data IRefProg =
    Deferred UID                -- Deferred lookup; done later
  | RS String                   -- Lifts a String into a RefProg
  | RConcat IRefProg IRefProg   -- Concatenates with two subprograms
  | Name                        -- The Symbol to insert the ShortName directly

data Reference = Reference
  { _u :: UID
  , rp :: RefProg
  , _ra :: LblType
  , _sn :: ShortName }
makeLenses ''Reference

instance HasUID        Reference where uid = u
instance HasRefAddress Reference where getRefAdd = ra
instance HasShortName  Reference where shortname = sn

name :: IRefProg
name = Name

(+::+) :: IRefProg -> IRefProg -> IRefProg
(+::+) = RConcat

raw :: String -> IRefProg
raw = RS

defer :: UID -> IRefProg
defer = Deferred

prepend :: String -> IRefProg
prepend s = RS s +::+ RS ": " +::+ Name
