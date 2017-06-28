module Drasil.Sections.AuxiliaryConstants 
  (valsOfAuxConstantsF) where

import Control.Lens ((^.))
import Language.Drasil
import qualified Drasil.SRS as SRS
import Data.Drasil.SentenceStructures (foldlSP)
import Data.Drasil.Concepts.Documentation
import qualified Data.Drasil.Concepts.Math as CM
import Data.Drasil.Utils(getS)

import Data.Maybe (isJust)
import Data.Drasil.Utils(getS)

import Language.Drasil
import qualified Data.Drasil.Concepts.Math as CM
import Data.Drasil.Concepts.Documentation

valsOfAuxConstantsF :: (NamedIdea a) => a ->[QDefinition] -> Integer -> Section
valsOfAuxConstantsF kWord listOfConstants tblNum = (SRS.valsOfAuxCons) [intro (kWord), tableOfConstants (listOfConstants) (tblNum)] []

--FIXME: general introduction?
intro :: (NamedIdea a) => a -> Contents
intro kWord = foldlSP [S "This section contains the standard values that are used for calculations in" +:+ short kWord]

tableOfConstants :: [QDefinition] -> Integer -> Contents
tableOfConstants f num = Table
  [S "Symbol", S "Description", S "Value", S "Unit"]
  (mkTable [getS, phrase, (\c -> E $ equat c), unit'2Contents] f)
  (titleize table_ +: S (show num) +:+ S "Auxiliary Constants")
  True

{-
tableOfConstantsNEW :: [QDefinition] -> Integer -> Contents
tableOfConstantsNEW constants tblNum = Table
  [at_start symbol_, at_start description, at_start value, at_start' CM.unit_]
  (mkTable
  [(\ch -> (\(Just t) -> (getS t)) (getSymb ch)),
  phrase constants,
  unit'2Contents]
  sls)
  (titleize table_ +: S (show tblNum) +:+ S "Auxiliary ConstantsNEW")
  False
  where sls = filter (isJust . getSymb) constants
  -}