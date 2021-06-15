module Drasil.Website where

import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)
import Database.Drasil (Block, ChunkDB, SystemInformation(SI), cdb,
  rdb, refdb, _authors, _concepts, _constants, _constraints, _purpose,
  _datadefs, _instModels, _configFiles, _defSequence, _inputs, _kind, _outputs, _quants, 
  _sys, _sysinfodb, _usedinfodb)
import Language.Drasil
import Utils.Drasil
import Drasil.DocLang
import qualified Data.Drasil.Concepts.Documentation as Doc (srs)


-- matching the format of an example 
-- feels really hacky, but just testing for now
srs :: Document
srs = mkWebsite
--srs = mkDoc mkSRS S.forT si

--mkWebsite :: SRSDecl -> SystemInformation -> Document
mkWebsite :: Document
mkWebsite =
    --Document   Title            author     [Section]
    Document (S websiteTitle) [{-no author-}] sections

sections :: [Section]
-- Section Title [SecCons] Reference
sections = headerSec ++ introSec ++ caseStudySec ++ exampleSec ++ docsSec ++ analysisSec ++ graphSec

--hack for si to work
webName, web :: CI
webName = commonIdeaWithDict "websiteName" (cn websiteTitle) "Drasil" []
web = commonIdeaWithDict "website" (cn "website") "web" []

si :: SystemInformation
si = SI SI {
    _sys         = webName,
    _kind        = web,
    _authors     = [],
    _quants      = [],
    _purpose     = [],
    _concepts    = [] :: [UnitaryConceptDict],
    _instModels  = [], -- FIXME; empty _instModels
    _datadefs    = [],
    _configFiles = [],
    _inputs      = [],
    _outputs     = [],
    _defSequence = [] :: [Block QDefinition],
    _constraints = [] :: [ConstrainedChunk],
    _constants   = [],
    _sysinfodb   = symbMap,
    _usedinfodb  = usedDB,
    refdb       = rdb [] []
}


symbMap :: ChunkDB
symbMap = cdb ([] :: [QuantityDict]) ([] :: [IdeaDict])
([] :: [ConceptChunk]) ([] :: [UnitDefn]) [] [] [] [] [] [] [] allRefs

usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) ([] :: [IdeaDict])
           ([] :: [ConceptChunk]) ([] :: [UnitDefn]) [] [] [] [] [] [] [] ([] :: [Reference])

allRefs :: [Reference]
allRefs = map rw sections ++ [imageRef, gitHubRef]

-- need section references
headerSecRef, introSecRef, caseStudySecRef, exampleSecRef, docsSecRef, analysisSecRef, graphSecRef :: Reference
headerSecRef = makeSecRef "Header" "Header"
introSecRef = makeSecRef "Introduction" "Introduction"
caseStudySecRef = makeSecRef "Case Study" "Case Study"
exampleSecRef = makeSecRef "Examples" "Examples"
docsSecRef = makeSecRef "Documentation" "Documentation"
analysisSecRef = makeSecRef "Analysis" "Analysis"
graphSecRef = makeSecRef "Dependency Graphs" "Dependency Graphs"

--Hardcoded version of existing website.

--header
headerSec :: Section
headerSec = section EmptyS [mkParagraph (gitHubInfoName +:+ makeRef2S gitHubRef), imageContent]

imageContent :: Contents
imageContent = mkFig imageRef $ fig EmptyS imagePath

imageRef :: Reference
imageRef = makeFigRef "Drasil"

gitHubRef :: Reference
gitHubRef = Reference "gitHubRepo" (URI gitHubInfoURL) (shortname "gitHubRepo") None

websiteTitle, gitHubInfoName :: String
gitHubInfoURL, imagePath :: FilePath
websiteTitle = "Drasil - Generate All the Things!"
gitHubInfoName = "Link to GitHub repository:"
gitHubInfoURL = "https://github.com/JacquesCarette/Drasil"
imagePath = "./website/images/Icon.png"

-- intro
introSec :: Section
introSec = section EmptyS (map (mkParagraph.S) [introParagraph1, introParagraph2]) []

introParagraph1, introParagraph2 :: String
introParagraph1 = "Drasil is a framework for generating all of the software artifacts from a stable knowledge base,
  focusing currently on scientific software. The main goals are to reduce knowledge duplication and
  improve traceability. The atifacts are generated from a common knowledge-base using recipes
  written in a Domain-Specific Language (DSL). These recipes allow us to specify which pieces of
  knowledge should be used in which artifacts, how to transform them, and more."
-- need sentence below?
introParagraph2 = "This webpage is designed to contain the most up to date" ++
  "case study artifacts, Haddock documentation, and package dependency graphs" --foldlList "," List [caseStudy, haddockDocs, packDepGraph] 
  ++ "from the Drasil repository.
  The case study artifacts include the Software Requirements Specification (SRS) for the case study,
  which specifies what the program sets out to achieve.
  The Haddock Documentation section contains the current documentation for the Drasil framework.
  The package dependency graphs shows the hierarchy of modules within each package.
  The footer of this page contains the continuous integration build of the project,
  as well as the commit number that the build and artifacts are based off of."

----------------------------------------------------
-- case studies section
caseStudiesTitle, caseStudiesDesc :: String
caseStudiesTitle = "Case Studies"
caseStudiesDesc = "Drasil allows some design decisions to be made by the user when generating 
  code. The table below summarizes the design decisions made for each case 
  study, followed by a guide giving the meaning of the short-forms used in the 
  table:"
-- case studies table
type Name = String
data Modularity = P | S deriving (Show)
data ImplementType = L | P deriving (Show)
data Logging = L | NoL deriving (Show)
data InStruct = B | U deriving (Show)
data ConStruct = B | I | U | WI deriving (Show)
data ConRep = C | V deriving (Show)
data RealNumRep = D | F deriving (Show)
data CaseStudy = CS Name Modularity ImplementType Logging InStruct ConStruct ConRep RealNumRep
glassBRCase      = CS "GlassBR"                     S P L   B I  C D
noPCMCase        = CS "NoPCM"                       C P NoL U B  C D
pdControllerCase = CS "PDController"                C P NoL U B  C D
projectileCase1  = CS "Projectile_C_P_NoL_B_U_V_D"  C P NoL B U  V D
projectileCase2  = CS "Projectile_S_L_NoL_U_U_V_F"  S L NoL U U  V F
projectileCase3  = CS "Projectile_U_P_L_B_B_C_D"    U P L   B B  C D
projectileCase4  = CS "Projectile_U_P_NoL_U_WI_V_D" U P NoL U WI V D
projectileCase5  = CS "Projectile_U_P_L_B_WI_V_F"   U P L   B WI V F

-- case studies symbol legend
modularityTitle, implementTypeTitle, loggingTitle, inStructTitle, conStructTitle, 
  conRepTitle, realNumRepTitle, modPt1, modPt2, modPt3, implementPt1, implementPt2, 
  logPt1, logPt2, inStructPt1, inStructPt2, conStructPt1, conStructPt2, conStructPt3, 
  conStructPt4, conRepPt1, conRepPt2, realNumRepPt1, realNumRepPt2 :: String

modularityTitle = "Modularity"
implementTypeTitle = "Implementation Type"
loggingTitle = "Logging"
inStructTitle = "Input Structure"
conStructTitle = "Constant Structure"
conRepTitle = "Constant Representation"
realNumRepTitle = "Real Number Representation"

modPt1 = "U - Unmodular"
modPt2 = "C - Modular with Combined input module"
modPt3 = "S - Modular with Separated input module"
implementPt1 = "P - Program"
implementPt2 = "L - Library"
logPt1 = "NoL - No Logging statements"
logPt2 = "L - Logging statements included"
inStructPt1 = "B - Inputs are Bundled in a class"
inStructPt2 = "U - Inputs are Unbundled"
conStructPt1 = "I - Constant values are Inlined<"
conStructPt2 = "WI - Constants are stored With the Inputs"
conStructPt3 = "B - Constants are stored in variables that are Bundled in a class"
conStructPt4 = "U - Constants are stored in variables that are Unbundled"
conRepPt1 = "V - Constants are stored as Variables"
conRepPt2 = "C - Constants are stored as Constants"
realNumRepPt1 = "D - Real numbers are represented as Doubles"
realNumRepPt2 = "F - Real numbers are represented as Floats"

----------------------------------------
-- example section
exampleIntro :: String
exampleIntro = "Each of the case studies contain their own generated PDF and HTML reports,
  and in some cases, their own generated code."

--example names, maybe make a unique type to accept fields of documents, gen code, and doxygen?
pendulum, gamePhys, glassBR, hghc, noPCM, pdController, projectile, ssp, swhs, template :: String

pendulum = "DblPendulum"
gamePhys = "Game Physics"
glassBR = "Glass BR"
hghc = "HGHC"
noPCM = "NoPCM"
pdController = "PDController"
projectile = "Projectile"
ssp = "SSP"
swhs = "SWHS"
template = "Template"

pendulumDesc, gamePhysDesc, glassBRDesc, hghcDesc, noPCMDesc, pdControllerDesc,
  projectileDesc, sspDesc, swhsDesc, templateDesc :: String

pendulumDesc = ""
gamePhysDesc = "describes the modeling of an open source 2D rigid body physics library used for games."
glassBRDesc = "predicts whether a given glass slab is likely to resist a specified blast."
hghcDesc = "describes heat transfer coefficients related to clad."
noPCMDesc = "describes the modelling of a solar water heating system without phase change material."
pdControllerDesc = ""
projectileDesc = "describes the motion of a projectile object in free space."
sspDesc = "describes the requirements of a slope stability analysis program."
swhsDesc = "describes the modelling of a solar water heating system with phase change material."
templateDesc = "an empty template document."

generatedCodeTitle, generatedCodeDocsTitle :: String
genCodeRootPath, exampleRoot :: FilePath
generatedCodeTitle = "Generated Code:"
generatedCodeDocsTitle = "Generated Code Documentation:"
genCodeRootPath = "https://github.com/JacquesCarette/Drasil/tree/master/code/stable"
exampleRoot = "examples"

----------------------------------------------------
--docs, analysis, graph sections
haddockDocsTitle, haddockDocsDesc, drasilDataTableTitle, dataTableDesc, packDepGraphTitle :: String
docsPath, fulldocsPath, dataTableHTMLPath, dataTableCSVPath :: FilePath
drasilFolders, drasilDepGraphPaths :: [String]

haddockDocsTitle = "Haddock Documentation"
haddockDocsDesc = "The current Haddock documentation for the Drasil framework. A variant with fully exposed modules is also available."
docsPath = "docs/index.html"
fullDocsPath = "docs/full/index.html"

drasilDataTableTitle = "Drasil Data Table"
dataTableDesc = "Here is the updated Data Table for the Drasil framework. There is also a downloadable version (csv format)."
dataTableHTMLPath = "analysis/DataTable.html"
dataTableCSVPath = "analysis/DataTable.csv"

packDepGraphTitle = "Package Dependency Graphs"
drasilFolders = ["drasil-build", "drasil-code", "drasil-data", "drasil-database", "drasil-docLang", "drasil-example", "drasil-gen", "drasil-gool", "drasil-lang", "drasil-printers", "drasil-theory", "drasil-utils"]
drasilDepGraphPaths = map (\x -> "graphs/" ++ x ++ ".pdf") drasilFolders



