module Language.Drasil.Chunk.Attribute 
  ( getSource, getDerivation, getShortName
  , shortname', sourceref
  ) where

import Control.Lens ((^.))
import Language.Drasil.Spec (Sentence(EmptyS, S), (+:+))
import Language.Drasil.Chunk.Attribute.Core (Attributes, Attribute(..))
import Language.Drasil.Chunk.Attribute.Derivation (Derivation)
import Language.Drasil.Classes (HasAttributes(attributes), HasDerivation(derivations),
  HasShortName(shortname), HasReference(getReferences))
import Language.Drasil.Chunk.Attribute.ShortName
import Language.Drasil.Chunk.Attribute.References

--------------------------------------------------------------------------------

-- Should this get only the first one or all potential sources?
-- Should we change the source ref to have a list (to keep things clean in case
--    of multiple sources)?
-- | Get the source reference from the references (if it exists)
getSource :: HasReference c => c -> Sentence
getSource c = sourceRef $ c ^. getReferences
  where
    sourceRef :: References -> Sentence
    sourceRef []                 = EmptyS
    sourceRef ((SourceRef x):xs) = x +:+ (sourceRef xs)

getDerivation :: HasDerivation c => c -> Derivation
getDerivation c =  c ^. derivations


getShortName :: HasShortName c => c -> Sentence
getShortName c = unwrap $ c ^. shortname
  where 
    unwrap :: ShortNm -> Sentence
    unwrap (ShortName s) = S s

sourceref :: Sentence -> Reference
sourceref = SourceRef

