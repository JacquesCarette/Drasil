module Language.Drasil.RefProg 
  (RefProg(..), Reference2(Reference2), name, (+::+), raw, defer, prepend,
   IRefProg(..))
  where
import Language.Drasil.RefTypes(RefAdd)
import Language.Drasil.ShortName (ShortName)
import Language.Drasil.UID (UID)

-- Trying different pieces of information for a reference
data RefProg = RP IRefProg | Citation

data IRefProg =
    Deferred UID                -- Deferred lookup; done later
  | RS String                   -- Lifts a String into a RefProg
  | RConcat IRefProg IRefProg   -- Concatenates with two subprograms
  | Name                        -- The Symbol to insert the ShortName directly
data Reference2 = Reference2 RefProg RefAdd ShortName

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
