module Language.Drasil.Chunk.Attribute 
  ( getSource, getDerivation, getShortName
  , shortname', snToSentence
  ) where

import Control.Lens ((^.))

import Language.Drasil.Derivation (Derivation)
import Language.Drasil.ShortName (ShortName, shortname', getStringSN)

import Language.Drasil.Classes (HasDerivation(derivations), HasReference(getReferences),
  HasShortName(shortname))
import Language.Drasil.RefTypes (Reference)
import Language.Drasil.Sentence (Sentence(EmptyS, S, Ref), (+:+), sC)

--------------------------------------------------------------------------------

-- Should this get only the first one or all potential sources?
-- Should we change the source ref to have a list (to keep things clean in case
--    of multiple sources)?
-- | Get the source reference from the references (if it exists)
getSource :: HasReference c => c -> Sentence
getSource c = foldList $ c ^. getReferences
  where
    foldList :: [Reference] -> Sentence
    foldList []       = EmptyS
    foldList [x]      = Ref x 
    foldList [x, y]   = Ref x +:+ S "and" +:+ Ref y 
    foldList (x:y:xs) = Ref x `sC` (foldList (y:xs))

getDerivation :: HasDerivation c => c -> Derivation
getDerivation c =  c ^. derivations

getShortName :: HasShortName c => c -> Sentence
getShortName c = snToSentence $ c ^. shortname

snToSentence :: ShortName -> Sentence
snToSentence = S . getStringSN
