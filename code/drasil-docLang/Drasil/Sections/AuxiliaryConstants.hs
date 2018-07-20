module Drasil.Sections.AuxiliaryConstants 
  (valsOfAuxConstantsF) where

import Language.Drasil
import qualified Drasil.DocLang.SRS as SRS (valsOfAuxCons)
import Data.Drasil.SentenceStructures (foldlSP)
import Data.Drasil.Concepts.Documentation (value, description, symbol_)
import qualified Data.Drasil.Concepts.Math as CM (unit_)
import Control.Lens ((^.))

valsOfAuxConstantsF :: (Idea a) => a ->[QDefinition] -> Section
valsOfAuxConstantsF kWord listOfConstants = SRS.valsOfAuxCons (contentGenerator kWord listOfConstants)  []

contentGenerator :: (Idea a) => a -> [QDefinition] -> [LabelledContent]
contentGenerator _ [] = [llcc "auxConstIntro" (mkLabelRA'' "auxConstIntroLabel") $ 
  foldlSP [S "There are no auxiliary constants"]]
contentGenerator a b  = [llcc "auxConstIntro" (mkLabelRA'' "auxConstIntroLabel") $
  intro a, tableOfConstants b]

--FIXME: general introduction?
intro :: (Idea a) => a -> Contents
intro kWord =  foldlSP [S "This section contains the standard values that are used for calculations in" +:+ short kWord]

tableOfConstants :: [QDefinition] -> LabelledContent
tableOfConstants f = llcc "TAuxConstsLC" (mkLabelRA'' "TAuxConsts") $
  Table
  [titleize symbol_, titleize description, titleize value, titleize CM.unit_]
  (mkTable [ch, phrase, (\c -> E $ c^.equat), unitToSentence] f)
  (S "Auxiliary Constants")
  True
  "TAuxConsts"