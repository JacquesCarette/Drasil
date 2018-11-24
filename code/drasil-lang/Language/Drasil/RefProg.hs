module Language.Drasil.RefProg (RefProg(..), Reference2(Reference2), name, (+::+), raw, defer, prepend) where
import Language.Drasil.RefTypes(RefAdd)
import Language.Drasil.ShortName (ShortName)
import Language.Drasil.UID (UID)

-- Trying different pieces of information for a reference
data RefProg =
    Deferred UID                -- Deferred lookup; done later
  | RS String                    -- Lifts a String into a RefProg
  | RConcat RefProg RefProg      -- Concatenates with two subprograms
  | Name                        -- The Symbol to insert the ShortName directly
data Reference2 = Reference2 RefProg RefAdd ShortName

name :: RefProg
name = Name

(+::+) :: RefProg -> RefProg -> RefProg
(+::+) = RConcat

raw :: String -> RefProg
raw = RS

defer :: UID -> RefProg
defer = Deferred

prepend :: String -> RefProg
prepend s = raw s +::+ raw ": " +::+ name
