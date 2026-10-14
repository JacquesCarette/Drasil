-- | Defines helper functions for making the Table of Auxiliary Constants section.
module Drasil.SRS.Sections.AuxiliaryConstants
  (valsOfAuxConstantsF, tableOfConstants, tableOfConstantsRef) where

-- General Haskell
import Control.Lens ((^.))

-- General Drasil
import Drasil.Database (HasUID(..))
import Language.Drasil
import Language.Drasil.Document
import Data.List.Extras (mkTable)
import Drasil.System (ProjectName, projAbrvS)

-- Other docLang
import qualified Drasil.SRS.Concepts as SRS (valsOfAuxCons)
import Drasil.SRS.DocumentLanguage.Units (toSentence) -- TODO: suspicious
import Drasil.SRS.Sections.ReferenceMaterial (emptySectSentPlu)

-- Vocabulary
import Drasil.Metadata.Documentation (value, description, symbol_, tAuxConsts)
import qualified Drasil.Metadata.Concepts.Math as CM (unit_)

-- | Gets the auxiliary constant values given an introductory 'Idea' and a 'QDefinition'.
valsOfAuxConstantsF :: ProjectName -> [ConstQDef] -> Section
valsOfAuxConstantsF kWord listOfConstants = SRS.valsOfAuxCons (contentGenerator kWord listOfConstants)  []

-- | Gets a table of constants from a 'QDefinition'. Also uses an 'Idea' as the introduction.
contentGenerator :: ProjectName -> [ConstQDef] -> [Contents]
contentGenerator _ [] = [mkParagraph $ emptySectSentPlu [tAuxConsts]]
contentGenerator a b  = [intro a, LlC $ tableOfConstants b]

--FIXME: general introduction?
-- | Helper that creates a general introduction using an 'Idea'.
intro :: ProjectName -> Contents
intro kWord = foldlSP [S "This section contains the standard values that are used for calculations in" +:+ projAbrvS kWord]

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
