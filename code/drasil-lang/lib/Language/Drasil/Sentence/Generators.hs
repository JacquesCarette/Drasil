-- | Functions that create sentences from other data
module Language.Drasil.Sentence.Generators (
  fromSource, fterms, getTandS,
  -- this does not belong here...
  checkValidStr
  ) where

import Control.Lens ((^.))

import Drasil.Database (IsChunk)

import Language.Drasil.Classes (NamedIdea(..), Quantity)
import Language.Drasil.Development.Sentence (phrase)
import Language.Drasil.Label.Type (Referable)
import Language.Drasil.NounPhrase.Types (NP)
import Language.Drasil.Reference (refS)
import Language.Drasil.Sentence (Sentence(S),(+:+), sParen, ch)
import Language.Drasil.ShortName (HasShortName(..))

-- | Wraps "from @reference@" in parentheses.
fromSource :: (Referable r, HasShortName r) => r -> Sentence
fromSource r = sParen (S "from" +:+ refS r)

-- | Apply a binary function to the terms of two named ideas, instead of to the named
-- ideas themselves. Ex. @fterms compoundPhrase t1 t2@ instead of
-- @compoundPhrase (t1 ^. term) (t2 ^. term)@.
fterms :: (NamedIdea c, NamedIdea d) => (NP -> NP -> t) -> c -> d -> t
fterms f a b = f (a ^. term) (b ^. term)

-- | Used when you want to say a term followed by its symbol. ex. "...using the Force F in...".
getTandS :: (IsChunk t, Quantity t) => t -> Sentence
getTandS a = phrase a +:+ ch a

-- | Uses an 'Either' type to check if a 'String' is valid -
-- 'Left' with error message if there is an invalid 'Char' in 'String', else 'Right' with 'String'.
checkValidStr :: String -> String -> Either String String
checkValidStr s [] = Right s
checkValidStr s (x:xs)
  | x `elem` s = Left $ "Invalid character: \'" ++ [x] ++ "\' in string \"" ++ s ++ ['\"']
  | otherwise  = checkValidStr s xs
