module Language.Drasil.Chunk.Attribute 
  ( Attributes, HasAttributes(..)
  , getSource, getDerivation, getShortName
  , shortname, sourceref, derivationsteps
  ) where

import Control.Lens (Lens', (^.))
import Language.Drasil.Spec (Sentence(EmptyS))
import Language.Drasil.Chunk.Attribute.Derivation

-- | Attributes are just a list of 'Attribute'
type Attributes = [Attribute]

-- | An attribute can be a rationale, a reference to the source (we used) to find
-- this knowledge, or a derivation to show how we arrived 
-- at a given model/definition/etc.
data Attribute =
    Rationale Sentence
  | ShortName Sentence
  | SourceRef Sentence -- Source to reference for this knowledge chunk
                       -- FIXME: Allow URLs/Citations here
  | D Derivation -- Makes sense for now 
        --(derivations are just document sections at the moment), 
        -- but we may need to create a new representation for it in the future.
        -- To collapse Attributes into QDefinitions, can't use Contents
  | Uses [String] -- Which chunks does this one rely on?

-- | Anything with 'Attributes'
class HasAttributes c where
  attributes :: Lens' c Attributes

--------------------------------------------------------------------------------

-- Should this get only the first one or all potential sources?
-- Should we change the source ref to have a list (to keep things clean in case
--    of multiple sources)?
-- | Get the source reference from the attributes (if it exists)
getSource :: HasAttributes c => c -> Sentence
getSource c = sourceRef $ c ^. attributes

sourceRef :: Attributes -> Sentence
sourceRef []                = EmptyS
sourceRef ((SourceRef x):_) = x
sourceRef (_:xs)            = sourceRef xs

getDerivation :: HasAttributes c => c -> Derivation
getDerivation c = deriv $ c ^. attributes
  where
    deriv :: Attributes -> Derivation
    deriv []          = []
    deriv ((D der):_) = der
    deriv (_:xs)      = deriv xs

getShortName :: HasAttributes c => c -> Maybe Sentence
getShortName c = shortName $ c ^. attributes
  where
    shortName :: Attributes -> Maybe Sentence
    shortName [] = Nothing
    shortName ((ShortName s):_) = Just s
    shortName (_:xs) = shortName xs

shortname, sourceref :: Sentence -> Attribute
shortname = ShortName
sourceref = SourceRef

derivationsteps :: Derivation -> Attribute
derivationsteps = D

