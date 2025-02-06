-- | Standard code to make a table of units.
-- First true example of a (small) recipe.
module Drasil.Sections.TableOfUnits (tOfUnitDesc, tOfUnitSIName, unitTableRef, tunit, tunit', tunitNone, tuIntro, defaultTUI) where

import Control.Lens ((^.))
import Language.Drasil
import Drasil.Sections.ReferenceMaterial(emptySectSentPlu)
import Data.Drasil.Concepts.Documentation (symbol_, description, tOfUnit)
import Drasil.DocumentLanguage.Core (TUIntro(..), RefTab(..))
import Data.Drasil.Concepts.Math (unit_)
import Utils.Drasil (mkTable)

-- | Creates the Table of Units with an "SI Name" column.
tOfUnitSIName :: IsUnit s => [s] -> LabelledContent
tOfUnitSIName = tOfUnitHelper [atStart symbol_, atStart description, S "SI Name"]
                  [Sy . usymb, (^. defn), phrase]

-- | Creates the Table of Units with SI name in the "Description" column.
tOfUnitDesc :: IsUnit s => [s] -> LabelledContent
tOfUnitDesc = tOfUnitHelper [atStart symbol_, atStart description]
                 [Sy . usymb, \x -> (x ^. defn) +:+ sParen (phrase x)]

tOfUnitNone :: [UnitDefn] -> LabelledContent
tOfUnitNone [] = tOfUnitHelper [] [] []
tOfUnitNone _ = tOfUnitHelper [] [] []

-- | Helper for making a Table of Units.
tOfUnitHelper :: [Sentence] -> [s -> Sentence] -> [s] -> LabelledContent
tOfUnitHelper _       _  [] = llcc unitTableRef $ Paragraph EmptyS
tOfUnitHelper []      _  _  = llcc unitTableRef $ Paragraph EmptyS
tOfUnitHelper _       [] _  = llcc unitTableRef $ Paragraph EmptyS
tOfUnitHelper headers fs u  = llcc unitTableRef $ Table headers
  (mkTable fs u) (S "Table of Units") True

-- | Makes a reference to the Table of Units.
unitTableRef :: Reference
unitTableRef = makeTabRef "ToU"

----- Table of units section helper functions -----

-- | Table of units constructors.
tunit, tunit', tunitNone :: [TUIntro] -> RefTab
-- | Table of units with an SI Name.
tunit  t = TUnits' t tOfUnitSIName
-- | Table of units with SI name in the description column.
tunit' t = TUnits' t tOfUnitDesc
-- | Table of units with SI name in the description column.
tunitNone t = TUnits' t tOfUnitNone

-- | Table of units introduction builder. Used by 'mkRefSec'.
tuIntro :: [TUIntro] -> Contents
tuIntro [] = mkParagraph $ emptySectSentPlu [unit_]
tuIntro x = mkParagraph $ foldr ((+:+) . tuI) EmptyS x

-- | Table of units introduction writer. Translates a 'TUIntro' to a 'Sentence'.
tuI :: TUIntro -> Sentence
tuI System  =
  S "The unit system used throughout is SI (Système International d'Unités)."
tuI TUPurpose =
  S "For each unit" `sC` S "the" +:+ namedRef unitTableRef (titleize' tOfUnit) +:+
    S "lists" +:+. foldlList Comma List [S "the" +:+ phrase symbol_,
    S "a" +:+ phrase description, S "the SI name"]
tuI Derived =
  S "In addition to the basic units, several derived units are also used."

-- | Default table of units intro that contains the system, derivation, and purpose.
defaultTUI :: [TUIntro]
defaultTUI = [System, Derived, TUPurpose]
