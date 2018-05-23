module Language.Drasil.Chunk.Attribute 
  ( getSource, getDerivation, getShortName
  , shortname', sourceref, derivationsteps
  ) where

import Control.Lens ((^.))
import Language.Drasil.Spec (Sentence(EmptyS))
import Language.Drasil.Chunk.Attribute.Core (Attributes, Attribute(..))
import Language.Drasil.Chunk.Attribute.Derivation (Derivation)
import Language.Drasil.Classes (HasAttributes(attributes))

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

shortname', sourceref :: Sentence -> Attribute
shortname' = ShortName
sourceref = SourceRef

derivationsteps :: Derivation -> Attribute
derivationsteps = D

