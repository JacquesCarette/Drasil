module Drasil.Sections.AuxiliaryConstants 
  (valsOfAuxConstantsF) where

import Language.Drasil
import Utils.Drasil
import qualified Drasil.DocLang.SRS as SRS (valsOfAuxCons)
import Drasil.DocumentLanguage.Units (toSentence)
import Data.Drasil.Concepts.Documentation (value, description, symbol_)
import qualified Data.Drasil.Concepts.Math as CM (unit_)
import Control.Lens ((^.))

valsOfAuxConstantsF :: (Idea a) => a ->[QDefinition] -> Section
valsOfAuxConstantsF kWord listOfConstants = SRS.valsOfAuxCons (contentGenerator kWord listOfConstants)  []

contentGenerator :: (Idea a) => a -> [QDefinition] -> [Contents]
contentGenerator _ [] = [foldlSP [S "There are no auxiliary constants"]]
contentGenerator a b  = [intro a, LlC $ tableOfConstants b]

--FIXME: general introduction?
intro :: (Idea a) => a -> Contents
intro kWord =  foldlSP [S "This section contains the standard values that are used for calculations in" +:+ short kWord]

tableOfConstants :: [QDefinition] -> LabelledContent
tableOfConstants f = llcc (makeTabRef "TAuxConsts") $ Table
  [titleize symbol_, titleize description, titleize value, titleize CM.unit_]
  (mkTable [ch, phrase, \c -> E $ c^.equat, toSentence] f)
  (S "Auxiliary Constants")
  True
