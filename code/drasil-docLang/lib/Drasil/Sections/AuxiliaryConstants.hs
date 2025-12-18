-- | Defines helper functions for making the Table of Auxiliary Constants section.
module Drasil.Sections.AuxiliaryConstants
  (valsOfAuxConstantsF, tableOfConstants, tableOfConstantsRef) where

import Control.Lens ((^.))

import Drasil.Database (HasUID(..))
import Language.Drasil
import qualified Drasil.DocLang.SRS as SRS (valsOfAuxCons)
import Drasil.DocumentLanguage.Units (toSentence)
import Data.Drasil.Concepts.Documentation (value, description, symbol_, tAuxConsts)
import qualified Data.Drasil.Concepts.Math as CM (unit_)
import Drasil.Sections.ReferenceMaterial (emptySectSentPlu)
import Utils.Drasil (mkTable)

-- | Gets the auxiliary constant values given an introductory 'Idea' and a 'QDefinition'.
valsOfAuxConstantsF :: Idea a => a -> [ConstQDef] -> Section
valsOfAuxConstantsF kWord listOfConstants = SRS.valsOfAuxCons (contentGenerator kWord listOfConstants)  []

-- | Gets a table of constants from a 'QDefinition'. Also uses an 'Idea' as the introduction.
contentGenerator :: Idea a => a -> [ConstQDef] -> [Contents]
contentGenerator _ [] = [mkParagraph $ emptySectSentPlu [tAuxConsts]]
contentGenerator a b  = [intro a, LlC $ tableOfConstants b]

--FIXME: general introduction?
-- | Helper that creates a general introduction using an 'Idea'.
intro :: (Idea a) => a -> Contents
intro kWord = foldlSP [S "This section contains the standard values that are used for calculations in" +:+ short kWord]

-- | Helper that gets a table of constants from a 'QDefinition'.
tableOfConstants :: [ConstQDef] -> LabelledContent
tableOfConstants f = mkRawLC (Table
  [titleize symbol_, titleize description, titleize value, titleize CM.unit_]
  (mkTable [ch, phrase, \c -> eS $ express $ c ^. defnExpr, toSentence] f)
  (titleize' tAuxConsts)
  True) tableOfConstantsRef

-- | Table of constants reference label.
tableOfConstantsRef :: Reference
tableOfConstantsRef = makeTabRef' (tAuxConsts ^. uid)
