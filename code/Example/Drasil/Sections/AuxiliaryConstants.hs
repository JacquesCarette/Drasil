module Drasil.Sections.AuxiliaryConstants 
  (valsOfAuxConstantsF) where

import Language.Drasil
import qualified Drasil.SRS as SRS
import Data.Drasil.SentenceStructures (foldlSP)
import Data.Drasil.Concepts.Documentation

valsOfAuxConstantsF :: (NamedIdea a) => a ->[QDefinition] -> Integer -> Section
valsOfAuxConstantsF kWord listOfConstants tblNum = (SRS.valsOfAuxCons) [intro (kWord), tableOfConstants (listOfConstants) (tblNum)] []

--FIXME: general introduction?
intro :: (NamedIdea a) => a -> Contents
intro kWord = foldlSP [S "This section contains the standard values that are used for calculations in" +:+ short kWord]

tableOfConstants :: [QDefinition] -> Integer -> Contents
tableOfConstants f num = Table
  [S "Constant Relation"]
  (mkTable [(\c -> E $ equat c)] f)
  (titleize table_ +: S (show num) +:+ S "Auxiliary Constants")
  True