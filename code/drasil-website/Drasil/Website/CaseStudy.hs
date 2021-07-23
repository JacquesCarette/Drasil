module Drasil.Website.CaseStudy (caseStudySec, caseStudyRefs, allExampleSI) where

import Language.Drasil hiding (E)
import Language.Drasil.Code
import Database.Drasil
import GOOL.Drasil (CodeType(..))

import qualified Drasil.DblPendulum.Body as DblPendulum (fullSI)
import qualified Drasil.GamePhysics.Body as GamePhysics (fullSI)
import qualified Drasil.GlassBR.Body as GlassBR (fullSI)
import qualified Drasil.HGHC.Body as HGHC (fullSI)
import qualified Drasil.NoPCM.Body as NoPCM (fullSI)
import qualified Drasil.PDController.Body as PDController (fullSI)
import qualified Drasil.Projectile.Body as Projectile (fullSI)
import qualified Drasil.SSP.Body as SSP (fullSI)
import qualified Drasil.SWHS.Body as SWHS (fullSI)
import qualified Drasil.Template.Body as Template (fullSI)

-- import choices for code generation
import qualified Drasil.GlassBR.Choices as GlassBR (choices)
import qualified Drasil.NoPCM.Choices as NoPCM (choices)
import qualified Drasil.PDController.Choices as PDController (codeChoices)
import qualified Drasil.Projectile.Choices as Projectile (codedDirName, choiceCombos)
-- the other examples currently do not generate any code.

-----------------------------
-- Case Studies Section
-----------------------------

caseStudySec :: Section
caseStudySec = section (S caseStudiesTitle) [mkParagraph $ S caseStudiesDesc, mkFig caseStudyTabRef mkCaseTable, UlC $ ulcc caseStudyLegend] [] caseStudySecRef

caseStudyRefs :: [Reference]
caseStudyRefs = [caseStudySecRef, ref caseStudySec, ref caseStudyTabRef]

caseStudySecRef :: Reference
caseStudySecRef = makeSecRef "CaseStudy" $ S caseStudyTitle

caseStudiesTitle, caseStudiesDesc :: String
caseStudiesTitle = "Case Studies"
caseStudiesDesc = "Drasil allows some design decisions to be made by the user when generating \
  \code. The table below summarizes the design decisions made for each case \
  \study, followed by a guide giving the meaning of the short-forms used in the \
  \table:"

mkCaseTable :: RawContent
mkCaseTable = Table headerRow (tableBody $ concatMap mkCaseStudy $ allExamples allExampleSI allExampleChoices)  EmptyS False

caseStudyTabRef :: Reference
caseStudyTabRef = makeTabRef "CaseStudy"

----- First Gather all SystemInformation and Choices from each example.

-- | Records example system information.
allExampleSI :: [SystemInformation]
allExampleSI = [DblPendulum.fullSI, GamePhysics.fullSI, GlassBR.fullSI, HGHC.fullSI, NoPCM.fullSI, PDController.fullSI, Projectile.fullSI, SSP.fullSI, SWHS.fullSI, Template.fullSI]

-- TODO: Automate this somehow. It seems a little too hard-coded.
-- To developer: Fill this list in when more examples can run code. The list
-- needs to be of this form since projectile comes with a list of choice combos.
-- | Records example choices. The order of the list must match up with
-- that in `allExampleSI`, or the Case Studies Table will be incorrect.
allExampleChoices :: [[Choices]]
allExampleChoices = [[], [], [GlassBR.choices], [], [NoPCM.choices], [PDController.codeChoices], Projectile.choiceCombos, [], [], []]

-- | Each Example gets placed in here.
data Example = E { sysInfoE :: SystemInformation,
                   choicesE :: [Choices]}

-- | Zip system info and choices from the examples.
allExamples :: [SystemInformation] -> [[Choices]] -> [Example]
allExamples = zipWith E

----- Then convert each example into its own case study. -----

-- | Holds individual case studies. System info may not be needed,
-- but it is still nice to keep around for now.
data CaseStudy = CS { sysInfoCS :: SystemInformation,
                      progName :: Sentence,
                      choicesCS :: Choices}

-- | Converts a list of examples into a list of CaseStudies. 
-- Currently, projectile is the only one that has more than one set of choices,
-- so we take the naming scheme from there.
mkCaseStudy :: Example -> [CaseStudy]
mkCaseStudy E{choicesE = []} = []
mkCaseStudy E{sysInfoE = si@SI{_sys = sys}, choicesE = [x]} = [CS{sysInfoCS = si, progName = S $ abrv sys, choicesCS = x}]
mkCaseStudy E{sysInfoE = si@SI{_sys = sys}, choicesE = xs} = map (\x -> CS{sysInfoCS = si, progName = S $ Projectile.codedDirName (abrv sys) x, choicesCS = x}) xs

----- After, convert each case study into a table to display. -----

--- We first need the helper functions to convert Choices into a displayable format (as a Sentence).
--- Those are defined in the section below to reduce clutter.
--- Then we make the header row, table body, and helper for the table body functions.

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
    modularity=md,
    impType=imp,
    logging = lg,
    inputStructure=instr,
    constStructure=constr,
    constRepr=conRep,
    spaceMatch=realNum
    }} = [nm, getMod md, getImp imp, getLog lg, getInstr instr, getConstr constr, getConRep conRep, getRealNum $ realNum Real]

--- Next, we need the legend to explain the Case Studies Table.
--- These functions are essentially hard-coded and also defined below.

-- | Each entry for the case studies table legend.
-- The title should be the same as the header.
data CSLegend = CSL {
  ttle :: String, -- String for now, should eventually move to at least a Sentence
  symbAndDefs :: [(String, String)]
}

---------------------------------------------------------------
-- Below functions create the legend for the Case Studies Table
---------------------------------------------------------------

-- | Make the legend for the case study table as a list.
caseStudyLegend :: RawContent
caseStudyLegend = Enumeration $ Bullet $ zip (map mkLegendListFunc legendEntries) $ repeat Nothing

mkLegendListFunc :: CSLegend -> ItemType
mkLegendListFunc csleg = Nested (S $ ttle csleg) $ Bullet $ zip (map mkTandDSent $ symbAndDefs csleg) $ repeat Nothing

-- Should eventually take Sentences instead of Strings. Converts into the format of "symbol - definition".
mkTandDSent :: (String, String) -> ItemType
mkTandDSent (sym,def) = Flat $ S sym +:+ S "-" +:+ S def


--- Case Study Table Headers

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

--- Case study legend entries

legendEntries :: [CSLegend]
legendEntries = [modularityLegend, implementationTypeLegend, loggingLegend, inputStrLegend, conStrLegend, conRepLegend, realNumRepLegend]

modularityLegend :: CSLegend
modularityLegend = CSL{
  ttle = modularityTitle,
  symbAndDefs = [ ("U", "Unmodular"),
                  ("C", "Modular with Combined input module"),
                  ("S", "Modular with Separated input module")]
}

implementationTypeLegend :: CSLegend
implementationTypeLegend = CSL {
  ttle = implementTypeTitle,
  symbAndDefs = [ ("P", "Program"),
                  ("L", "Library")]
}

loggingLegend :: CSLegend
loggingLegend = CSL {
  ttle = inStructTitle,
  symbAndDefs = [ ("NoL", "No Logging statements"),
                  ("L", "Logging statements included")]
}

inputStrLegend :: CSLegend
inputStrLegend = CSL {
  ttle = loggingTitle,
  symbAndDefs = [ ("B", "Inputs are Bundled in a class"),
                  ("U", "Inputs are Unbundled")]
}

conStrLegend :: CSLegend
conStrLegend = CSL {
  ttle = conStructTitle,
  symbAndDefs = [ ("I", "Constant values are Inlined"),
                  ("WI", "Constants are stored With the Inputs"),
                  ("B", "Constants are stored in variables that are Bundled in a class"),
                  ("U", "Constants are stored in variables that are Unbundled")]
}

conRepLegend :: CSLegend
conRepLegend = CSL {
  ttle = conRepTitle,
  symbAndDefs = [ ("V", "Constants are stored as Variables"),
                  ("C", "Constants are stored as Constants")]
}

realNumRepLegend :: CSLegend
realNumRepLegend = CSL {
  ttle = realNumRepTitle,
  symbAndDefs = [ ("D", "Real numbers are represented as Doubles"),
                  ("F", "Real numbers are represented as Floats")]
}


--------------------------------------------------------
-- Helper functions to create the case study table rows.
--------------------------------------------------------

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
getRealNum _ = error "This shouldn't happen. Make sure Real numbers have a preferred type"