-- | Even though we do not have 'Label's per se, here we define the
-- different ways of construction  ways to mark labels.
module Language.Drasil.Label.Type(
  -- types
    LblType(RP, Citation, URI), IRefProg(..)
  -- LblType accessor
  , getAdd
  -- IRefProg constructors
  , name, (+::+), raw, defer, prepend
  ) where

import Language.Drasil.UID (UID)

-- | Trying different pieces of information for a reference
-- An RP is a decorated internal reference
-- Citation is a citation
-- URI is for URLs and other external links
data LblType = RP IRefProg String | Citation String | URI String

data IRefProg =
    Deferred UID                -- Deferred lookup; done later
  | RS String                   -- Lifts a String into a RefProg
  | RConcat IRefProg IRefProg   -- Concatenates with two subprograms
  | Name                        -- The Symbol to insert the ShortName directly

getAdd :: LblType -> String
getAdd (RP _ s)     = s
getAdd (Citation s) = s
getAdd (URI s)      = s

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

