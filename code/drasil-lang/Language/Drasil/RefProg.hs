{-# Language TemplateHaskell #-}
module Language.Drasil.RefProg 
  (RefProg(..), Reference(Reference), name, (+::+), raw, defer, prepend,
   IRefProg(..), repUnd,
   makeGDRef, makeTabRef, makeFigRef, makeInstRef, makeDDRef, makeCiteRef,
   makeSecRef, makeLstRef, makeTMRef)
  where
import Language.Drasil.Classes.Core (HasUID(uid), HasRefAddress(getRefAdd),
  HasShortName(shortname))
import Language.Drasil.ShortName (ShortName, shortname')
import Language.Drasil.UID (UID)
import Language.Drasil.Label.Type (LblType(RefAdd))

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

-- FIXME: Duplicated from Document.hs!
repUnd :: String -> String
repUnd = map rep
  where
    rep :: Char -> Char
    rep '_' = '.'
    rep c = c

-- FIXME: horrible hacks.
makeGDRef :: String -> Reference
makeGDRef rs = Reference rs (RP $ prepend "GD") (RefAdd $ "GD:" ++ repUnd rs) (shortname' rs)

makeTabRef :: String -> Reference
makeTabRef rs = Reference rs (RP $ prepend "Tab") (RefAdd $ "Table:" ++ repUnd rs) (shortname' rs)

makeFigRef :: String -> Reference
makeFigRef rs = Reference rs (RP $ prepend "Fig") (RefAdd $ "Figure:" ++ repUnd rs) (shortname' rs)

makeInstRef :: String -> Reference
makeInstRef rs = Reference rs (RP $ prepend "IM") (RefAdd $ "IM:" ++ repUnd rs) (shortname' rs)

makeDDRef :: String -> Reference
makeDDRef rs = Reference rs (RP $ prepend "DD") (RefAdd $ "DD:" ++ repUnd rs) (shortname' rs)

makeTMRef :: String -> Reference
makeTMRef rs = Reference rs (RP $ prepend "TM") (RefAdd $ "T:" ++ repUnd rs) (shortname' rs)

makeCiteRef :: String -> Reference
makeCiteRef rs = Reference rs Citation (RefAdd $ repUnd rs) (shortname' rs)

makeSecRef :: String -> String -> Reference
makeSecRef r s = Reference (r ++ "Label") (RP $ prepend "Sect") (RefAdd $ "Sec:" ++ repUnd r) 
  (shortname' s)

makeLstRef :: String -> String -> Reference
makeLstRef r s = Reference (r ++ "Label") (RP $ prepend "Lst") (RefAdd $ "Lst:" ++ repUnd r) 
  (shortname' s)
