module Language.Drasil.Chunk.Attribute 
  ( getSource, getDerivation, getShortName
  , shortname', sourceref, derivationsteps
  ) where

import Control.Lens ((^.))
import Language.Drasil.Spec (Sentence(EmptyS))
import Language.Drasil.Chunk.Attribute.Core (Attributes, Attribute(..))
import Language.Drasil.Chunk.Attribute.Derivation (Derivation)
import Language.Drasil.Classes (HasAttributes(attributes), HasShortName(shortname))
import Language.Drasil.Chunk.Attribute.ShortName

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


getShortName :: HasShortName c => c -> Sentence
getShortName c = unwrap $ c ^. shortname
  where 
    unwrap :: ShortNm -> Sentence
    unwrap (ShortName s) = s

sourceref :: Sentence -> Attribute
sourceref = SourceRef

shortname' :: Sentence -> ShortNm
shortname' = ShortName

derivationsteps :: Derivation -> Attribute
derivationsteps = D

