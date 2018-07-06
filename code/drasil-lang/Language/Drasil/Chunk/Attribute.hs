module Language.Drasil.Chunk.Attribute 
  ( getSource, getDerivation, getShortName
  , shortname', sourceref, snToSentence
  ) where

import Control.Lens ((^.))
import Language.Drasil.Spec (Sentence(EmptyS, S), (+:+))
import Language.Drasil.Chunk.Derivation (Derivation)
import Language.Drasil.Chunk.References (Reference(SourceRef), References)
import Language.Drasil.Chunk.ShortName (ShortName(ShortNm), HasShortName(shortname), shortname')
import Language.Drasil.Classes (HasDerivation(derivations), HasReference(getReferences))

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
    sourceRef (SourceRef x:xs) = x +:+ sourceRef xs

getDerivation :: HasDerivation c => c -> Derivation
getDerivation c =  c ^. derivations

getShortName :: HasShortName c => c -> Sentence
getShortName c = snToSentence $ shortname c


snToSentence :: ShortName -> Sentence
snToSentence (ShortNm s) = S s



sourceref :: Sentence -> Reference
sourceref = SourceRef
