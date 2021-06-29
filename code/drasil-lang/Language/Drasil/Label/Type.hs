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

-- | Applying different pieces of information for a reference.
-- An RP is a decorated internal reference.
-- Citation is a citation.
-- URI is for URLs and other external links.
data LblType = RP IRefProg String | Citation String | URI String

-- | Created for different forms of references. Used in 'LblType'. 
data IRefProg =
    Deferred UID                -- ^ Deferred lookup; done later. Used for domains in a 'ConceptInstance'.
  | RS String                   -- ^ Lifts a 'String' into a 'RefProg'.
  | RConcat IRefProg IRefProg   -- ^ Concatenates with two subprograms.
  | Name                        -- ^ The 'Symbol' to insert the 'ShortName' directly.

-- | Retrieves the 'String' contained in a 'LblType'.
getAdd :: LblType -> String
getAdd (RP _ s)     = s
getAdd (Citation s) = s
getAdd (URI s)      = s

-- | Constructor that gets the 'Name' of an 'IRefProg'.
name :: IRefProg
name = Name

-- | Constructor that concatenates two subprograms.
(+::+) :: IRefProg -> IRefProg -> IRefProg
(+::+) = RConcat

-- | Constructor for a 'String' into an 'IRefProg'.
raw :: String -> IRefProg
raw = RS

-- | Constructor to defer a 'UID' lookup; done later.
defer :: UID -> IRefProg
defer = Deferred

-- | Prepends a 'String' to an 'IRefProg'.
prepend :: String -> IRefProg
prepend s = RS s +::+ RS ":" +::+ Name

