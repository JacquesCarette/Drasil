module Language.Drasil.Chunk.Attribute 
  ( getSource, getDerivation, getShortName
  , shortname', sourceref
  ) where

import Control.Lens ((^.))
import Language.Drasil.Spec (Sentence(EmptyS, S), (+:+))
import Language.Drasil.Chunk.Attribute.Core (Attributes, Attribute(..))
import Language.Drasil.Chunk.Attribute.Derivation (Derivation)
import Language.Drasil.Classes (HasAttributes(attributes), HasDerivation(derivations), 
  HasReference(getReferences))
import Language.Drasil.Chunk.Attribute.References
import Language.Drasil.Chunk.Attribute.ShortName

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
    sourceRef (_:xs)             = sourceRef xs

getDerivation :: HasDerivation c => c -> Derivation
getDerivation c =  c ^. derivations

getShortName :: HasShortName c => c -> Sentence
getShortName c = unwrap $ shortname c
  where
    unwrap :: ShortName -> Sentence
    unwrap (ShortNm s) = S s

{-getShortName :: HasShortName c => c -> Sentence
getShortName c = unwrap $ c ^. shortname
  where 
    unwrap (ShortNm s) = S s-}

sourceref :: Sentence -> Reference
sourceref = SourceRef