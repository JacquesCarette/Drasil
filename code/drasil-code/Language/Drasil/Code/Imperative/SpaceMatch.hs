module Language.Drasil.Code.Imperative.SpaceMatch (
  chooseSpace
) where

import Language.Drasil.Choices (Choices(..), MatchedSpaces)
import Language.Drasil.Code.Lang (Lang(..))

import GOOL.Drasil (CodeType(..))

-- Concretizes the SpaceMatch in Choices to a MatchedSpace based on target language
chooseSpace :: Lang -> Choices -> MatchedSpaces
chooseSpace lng chs = \s -> selectType lng s (spaceMatch chs s)
        -- Floats unavailable in Python
  where selectType Python s (Float:ts) = selectType Python s ts
        -- In all other cases, just select first choice
        selectType _ _ (t:_) = t
        selectType l s [] = error $ "Chosen CodeType matches for Space " ++ 
          show s ++ " are not compatible with target language " ++ show l