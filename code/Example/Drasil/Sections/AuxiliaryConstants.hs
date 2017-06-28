module Drasil.Sections.AuxiliaryConstants 
  (valsOfAuxConstantsF) where

import Language.Drasil
import qualified Drasil.SRS as SRS
import Data.Drasil.SentenceStructures (foldlSP)
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Utils(getS)
import qualified Data.Drasil.Concepts.Math as CM

valsOfAuxConstantsF :: (NamedIdea a) => a ->[QDefinition] -> Integer -> Section
valsOfAuxConstantsF kWord listOfConstants tblNum = (SRS.valsOfAuxCons) [intro (kWord), tableOfConstants (listOfConstants) (tblNum)] []

--FIXME: general introduction?
intro :: (NamedIdea a) => a -> Contents
intro kWord = foldlSP [S "This section contains the standard values that are used for calculations in" +:+ short kWord]

tableOfConstants :: [QDefinition] -> Integer -> Contents
tableOfConstants f num = Table
  [titleize symbol_, titleize description, titleize value, titleize CM.unit_]
  (mkTable [getS, phrase, (\c -> E $ equat c), unit'2Contents] f)
  (titleize table_ +: S (show num) +:+ S "Auxiliary Constants")
  True