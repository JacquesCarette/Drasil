module Language.Drasil.Code.Imperative.SpaceMatch (
  chooseSpace
) where

import Language.Drasil
import Language.Drasil.Choices (Choices(..), Maps(..))
import Language.Drasil.Code.Imperative.DrasilState (GenState, MatchedSpaces,
  addToDesignLog, addLoggedSpace)
import Language.Drasil.Code.Lang (Lang(..))

import GOOL.Drasil (CodeType(..))

import Control.Monad.State (modify)
import Text.PrettyPrint.HughesPJ (Doc, text)

-- | Concretizes the 'spaceMatch' in 'Choices' to a 'MatchedSpace' based on target language.
chooseSpace :: Lang -> Choices -> MatchedSpaces
chooseSpace lng chs = \s -> selectType lng s (spaceMatch (maps chs) s)
        -- Floats unavailable in Python
  where selectType :: Lang -> Space -> [CodeType] -> GenState CodeType
        selectType Python s (Float:ts) = do
          modify (addLoggedSpace s Float .
            addToDesignLog s Float (incompatibleType Python s Float))
          selectType Python s ts
        -- In all other cases, just select first choice
        selectType _ s (t:_) = do
          modify (addLoggedSpace s t .
            addToDesignLog s t (successLog s t))
          return t
        selectType l s [] = error $ "Chosen CodeType matches for Space " ++
          show s ++ " are not compatible with target language " ++ show l

-- | Defines a design log message based on an incompatibility between the given
-- 'Lang' and attempted 'Space'-'CodeType' match.
incompatibleType :: Lang -> Space -> CodeType -> Doc
incompatibleType l s t = text $ "Language " ++ show l ++ " does not support "
  ++ "code type " ++ show t ++ ", chosen as the match for the " ++ show s ++
  " space. Trying next choice."

-- | Defines a successful log message.
successLog :: Space -> CodeType -> Doc
successLog s t = text ("Successfully matched "++show s ++ " with "++ show t ++".")
