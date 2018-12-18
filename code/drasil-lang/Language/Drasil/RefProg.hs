{-# Language TemplateHaskell #-}
module Language.Drasil.RefProg 
  (Reference(Reference),
   repUnd,
   makeGDRef, makeTabRef, makeFigRef, makeInstRef, makeDDRef, makeCiteRef,
   makeSecRef, makeLstRef, makeTMRef, makeURI, makeAssumpRef)
  where
import Language.Drasil.Classes.Core (HasUID(uid), HasRefAddress(getRefAdd),
  HasShortName(shortname))
import Language.Drasil.ShortName (ShortName, shortname')
import Language.Drasil.UID (UID)
import Language.Drasil.Label.Type (LblType(RP,Citation, URI),
  -- name, (+::+), raw, defer, 
  prepend)

import Control.Lens (makeLenses)

data Reference = Reference
  { _ui :: UID
  ,  ra :: LblType     -- the main string of the reference address
  , _sn :: ShortName } -- the human-readable short name
makeLenses ''Reference

instance HasUID        Reference where uid = ui
instance HasRefAddress Reference where getRefAdd = ra
instance HasShortName  Reference where shortname = sn

-- FIXME: Duplicated from Document.hs!
repUnd :: String -> String
repUnd = map rep
  where
    rep :: Char -> Char
    rep '_' = '.'
    rep c = c

-- FIXME: horrible hacks.
makeGDRef :: String -> Reference
makeGDRef rs = Reference rs (RP (prepend "GD") ("GD:" ++ repUnd rs)) (shortname' rs)

makeTabRef :: String -> Reference
makeTabRef rs = Reference rs (RP (prepend "Tab") ("Table:" ++ repUnd rs)) (shortname' rs)

makeFigRef :: String -> Reference
makeFigRef rs = Reference rs (RP (prepend "Fig") ("Figure:" ++ repUnd rs)) (shortname' rs)

makeInstRef :: String -> Reference
makeInstRef rs = Reference rs (RP (prepend "IM") ("IM:" ++ repUnd rs)) (shortname' rs)

makeDDRef :: String -> Reference
makeDDRef rs = Reference rs (RP (prepend "DD") ("DD:" ++ repUnd rs)) (shortname' rs)

makeTMRef :: String -> Reference
makeTMRef rs = Reference rs (RP (prepend "TM") ("T:" ++ repUnd rs)) (shortname' rs)

makeCiteRef :: String -> Reference
makeCiteRef rs = Reference rs (Citation $ repUnd rs) (shortname' rs)

makeSecRef :: String -> String -> Reference
makeSecRef r s = Reference (r ++ "Label") (RP (prepend "Section") ("Sec:" ++ repUnd r))
  (shortname' s)

makeLstRef :: String -> String -> Reference
makeLstRef r s = Reference (r ++ "Label") (RP (prepend "Lst") ("Lst:" ++ repUnd r))
  (shortname' s)

makeAssumpRef :: String -> Reference
makeAssumpRef rs = Reference rs (RP (prepend "A") ("A:" ++ repUnd rs)) (shortname' rs)

-- | Create a reference for a URI
makeURI :: UID -> String -> ShortName -> Reference
makeURI u r s = Reference u (URI r) s
