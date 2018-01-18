module Drasil.Sections.AuxiliaryConstants 
  (valsOfAuxConstantsF) where

import Language.Drasil
import qualified Drasil.SRS as SRS
import Data.Drasil.SentenceStructures (foldlSP)
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Utils(getES)
import qualified Data.Drasil.Concepts.Math as CM

valsOfAuxConstantsF :: (NamedIdea a) => a ->[QDefinition] -> Section
valsOfAuxConstantsF kWord listOfConstants = (SRS.valsOfAuxCons) (contentGenerator kWord listOfConstants)  []

contentGenerator :: (NamedIdea a) => a -> [QDefinition] -> [Contents]
contentGenerator _ [] = [foldlSP [S "There are no auxiliary constants"]]
contentGenerator a b  = [intro a, tableOfConstants b]

--FIXME: general introduction?
intro :: (NamedIdea a) => a -> Contents
intro kWord = foldlSP [S "This section contains the standard values that are used for calculations in" +:+ short kWord]

tableOfConstants :: [QDefinition] -> Contents
tableOfConstants f = Table
  [titleize symbol_, titleize description, titleize value, titleize CM.unit_]
  (mkTable [getES, phrase, (\c -> E $ equat c), unit'2Contents] f)
  (S "Auxiliary Constants")
  True
  "TAuxConsts"
