{-# LANGUAGE TupleSections #-}
-- | Case Studies table for the different choices available when generating code from Drasil.
-- To be used in the Drasil website.
module Drasil.Website.CaseStudy where

import Language.Drasil hiding (E, Var)
import Language.Drasil.Code
import SysInfo.Drasil
import GOOL.Drasil (CodeType(..))

import Drasil.Website.Example (examples, Example(..))
import qualified Drasil.Projectile.Choices as Projectile (codedDirName)


-- * Case Studies Section

-- | Creates the Case Study Section.
caseStudySec :: Section
caseStudySec =
  section (S caseStudiesTitle) -- Title
  [mkParagraph $ S caseStudiesDesc, mkFig (makeTabRef "CaseStudy") mkCaseTable,
    mkParagraph $ S legendIntro, UlC $ ulcc caseStudyLegend] -- Contents
  [] $ makeSecRef "CaseStudy" $ S caseStudiesTitle -- Section Reference

caseStudiesTitle, caseStudiesDesc, legendIntro :: String
-- | Section title.
caseStudiesTitle = "Case Studies"
-- | Section description.
caseStudiesDesc = "Drasil allows some design decisions to be made by the user when generating \
  \code. The table below summarizes the design decisions made for each case \
  \study, followed by a guide giving the meaning of the short-forms used in the \
  \table:"
-- | Introduce the Case Study Table Legend as a list.
legendIntro = "The legend for the Case Studies Table is listed below according to column header:"

-- | Creates the Case Study Table
mkCaseTable :: RawContent
mkCaseTable = Table headerRow (tableBody $ concatMap mkCaseStudy $ examples "" "")  EmptyS False

-- * Manipulating info from 'Example' -> 'CaseStudy'
--
-- $ExampleToCaseStudy
--
-- After taking the information about the examples from Example.hs,
-- convert each example into its own case study.

-- | Holds individual case studies. System info may not be needed,
-- but it is still nice to keep around for now.
data CaseStudy = CS {
  -- | Each case study needs a name, so use system information. 
  sysInfoCS :: SystemInformation,
  -- | A case study may have different program names for the same example (ex. Projectile).
  progName :: Sentence,
  -- | Each case study has code that is generated from a set of choices.
  choicesCS :: Choices}

-- | Converts a list of examples into a list of CaseStudies. 
-- Currently, projectile is the only one that has more than one set of choices,
-- so we take the naming scheme from there.
mkCaseStudy :: Example -> [CaseStudy]
mkCaseStudy E{choicesE = []} = []
mkCaseStudy E{sysInfoE = si@SI{_sys = sys}, choicesE = [x]} = [CS{sysInfoCS = si, progName = S $ abrv sys, choicesCS = x}]
mkCaseStudy E{sysInfoE = si@SI{_sys = sys}, choicesE = xs} = map (\x -> CS{sysInfoCS = si, progName = S $ Projectile.codedDirName (abrv sys) x, choicesCS = x}) xs

-- * Display 'CaseStudy' Information as a Table
--
-- $CaseStudy
--
-- We first need the helper functions to convert 'Choices' into a displayable format (as a 'Sentence').
-- Those are defined in the section below to reduce clutter.
-- Then we make the header row, table body, and helper for the table body functions.

-- | Hardcoded header row for the Case studies table
headerRow :: [Sentence]
headerRow = map S [caseStudyTitle, modularityTitle, implementTypeTitle, loggingTitle, inStructTitle, conStructTitle, conRepTitle, realNumRepTitle]

-- | Creates the case study table body.
tableBody :: [CaseStudy] -> [[Sentence]]
tableBody = map displayCS

-- | Converts a case study into a table row for easy display.
displayCS :: CaseStudy -> [Sentence]
displayCS CS{progName = nm,
  choicesCS = Choices{
    architecture = a,
    dataInfo = d,
    maps = m,
    optFeats = o
    }} = [nm, getMod $ modularity a, getImp $ impType a, getLog $ logging $ logConfig o,
          getInstr $ inputStructure d, getConstr $ constStructure d, getConRep $ constRepr d,
           getRealNum (spaceMatch m Real)]

-- * Case Studies Table Legend
--
-- $CSLegend
--
-- Next, we need the legend to explain the Case Studies Table.
-- These functions are essentially hard-coded and also defined below.

-- | Each entry for the case studies table legend.
-- The title should be the same as the header.
data CSLegend = CSL {
  -- | Legend title.
  ttle :: String, -- String for now, should eventually move to at least a Sentence
  -- | Legend symbols along with their respective definitions.
  symbAndDefs :: [(String, String)]
}

-- | Make the legend for the case study table as a list.
caseStudyLegend :: RawContent
caseStudyLegend = Enumeration $ Bullet $ map ((, Nothing) . mkLegendListFunc) legendEntries

-- | Helper to convert the Case Study legends into list items.
mkLegendListFunc :: CSLegend -> ItemType
mkLegendListFunc csleg = Nested (S $ ttle csleg) $ Bullet $ map ((, Nothing) . mkTandDSent) $ symbAndDefs csleg

-- | Should eventually take Sentences instead of Strings. Converts into the format of "symbol - definition".
mkTandDSent :: (String, String) -> ItemType
mkTandDSent (sym,def) = Flat $ S sym +:+ S "-" +:+ S def

-- | Case Study Table column headers.
caseStudyTitle, modularityTitle, implementTypeTitle, loggingTitle, inStructTitle, conStructTitle,
  conRepTitle, realNumRepTitle :: String

caseStudyTitle = "Case Study"
modularityTitle = "Modularity"
implementTypeTitle = "Implementation Type"
loggingTitle = "Logging"
inStructTitle = "Input Structure"
conStructTitle = "Constant Structure"
conRepTitle = "Constant Representation"
realNumRepTitle = "Real Number Representation"

-- | Case study legend entries.
legendEntries :: [CSLegend]
legendEntries = [modularityLegend, implementationTypeLegend, loggingLegend, inputStrLegend, conStrLegend, conRepLegend, realNumRepLegend]

-- | Modularity or Separation of software.
modularityLegend :: CSLegend
modularityLegend = CSL{
  ttle = modularityTitle,
  symbAndDefs = [ ("U", "Unmodular"),
                  ("C", "Modular with Combined input module"),
                  ("S", "Modular with Separated input module")]
}

-- | Software implementation type.
implementationTypeLegend :: CSLegend
implementationTypeLegend = CSL {
  ttle = implementTypeTitle,
  symbAndDefs = [ ("P", "Program"),
                  ("L", "Library")]
}

-- | Compiler logging statements.
loggingLegend :: CSLegend
loggingLegend = CSL {
  ttle = loggingTitle,
  symbAndDefs = [ ("NoL", "No Logging statements"),
                  ("L", "Logging statements included")]
}

-- | Input value structure.
inputStrLegend :: CSLegend
inputStrLegend = CSL {
  ttle = inStructTitle,
  symbAndDefs = [ ("B", "Inputs are Bundled in a class"),
                  ("U", "Inputs are Unbundled")]
}

-- | Constant value structure.
conStrLegend :: CSLegend
conStrLegend = CSL {
  ttle = conStructTitle,
  symbAndDefs = [ ("I", "Constant values are Inlined"),
                  ("WI", "Constants are stored With the Inputs"),
                  ("B", "Constants are stored in variables that are Bundled in a class"),
                  ("U", "Constants are stored in variables that are Unbundled")]
}

-- | Constant value representation.
conRepLegend :: CSLegend
conRepLegend = CSL {
  ttle = conRepTitle,
  symbAndDefs = [ ("V", "Constants are stored as Variables"),
                  ("C", "Constants are stored as Constants")]
}

-- | Real number representation.
realNumRepLegend :: CSLegend
realNumRepLegend = CSL {
  ttle = realNumRepTitle,
  symbAndDefs = [ ("D", "Real numbers are represented as Doubles"),
                  ("F", "Real numbers are represented as Floats")]
}

-- * Helper functions to create the case study table rows.
--
-- $helpCSRow
--
-- These functions act like a version of 'show' for each
-- different type of 'Choices', but tweaked to fit inside a table.

getMod :: Modularity -> Sentence
getMod Unmodular = S "U"
getMod (Modular Combined) = S "C"
getMod (Modular Separated) = S "S"

getImp :: ImplementationType -> Sentence
getImp Program = S "P"
getImp Library = S "L"

getLog :: [Logging] -> Sentence
getLog [] = S "NoL"
getLog _ = S "L"

getInstr :: Structure -> Sentence
getInstr Bundled = S "B"
getInstr Unbundled = S "U"

getConstr :: ConstantStructure -> Sentence
getConstr Inline = S "I"
getConstr WithInputs = S "WI"
getConstr (Store Bundled) = S "B"
getConstr (Store Unbundled) = S "U"

getConRep :: ConstantRepr -> Sentence
getConRep Var = S "V"
getConRep Const = S "C"

getRealNum :: [CodeType] -> Sentence
getRealNum (Double:_) = S "D"
getRealNum (Float:_) = S "F"
getRealNum _ = error "This shouldn't happen. Make sure Real numbers have a preferred type."