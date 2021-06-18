module Drasil.Website.Website where

import Data.List (zipWith4)
import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)
import Database.Drasil (Block, ChunkDB, SystemInformation(SI), cdb,
  rdb, refdb, _authors, _concepts, _constants, _constraints, _purpose,
  _datadefs, _instModels, _configFiles, _defSequence, _inputs, _kind, _outputs, _quants, 
  _sys, _sysinfodb, _usedinfodb)
import Language.Drasil hiding (C)
import Utils.Drasil
--import Drasil.DocLang
--import qualified Data.Drasil.Concepts.Documentation as Doc (srs)


-- matching the format of an example 
-- feels really hacky, but just testing for now
--srs :: Document
--srs = mkWebsite
--srs = mkDoc mkSRS S.forT si

printSetting :: PrintingInformation
printSetting = PI symbMap Equational defaultConfiguration

--mkWebsite :: SRSDecl -> SystemInformation -> Document
mkWebsite :: Document
mkWebsite =
    --Document   Title            author     [Section]
    Document (S websiteTitle) EmptyS sections

si :: SystemInformation
si = SI {
    _sys         = webName,
    _kind        = web,
    _authors     = [] :: [Person],
    _quants      = [] :: [QuantityDict],
    _purpose     = [],
    _concepts    = [] :: [UnitaryConceptDict],
    _instModels  = [], -- :: [InstanceModel],
    _datadefs    = [], -- :: [DataDefinition],
    _configFiles = [],
    _inputs      = [] :: [QuantityDict],
    _outputs     = [] :: [QuantityDict],
    _defSequence = [] :: [Block QDefinition],
    _constraints = [] :: [ConstrainedChunk],
    _constants   = [] :: [QDefinition],
    _sysinfodb   = symbMap,
    _usedinfodb  = usedDB,
    refdb        = rdb [] []
}

sections :: [Section]
-- Section Title [SecCons] Reference
sections = [headerSec, introSec, caseStudySec, exampleSec, docsSec, analysisSec, graphSec]

symbMap :: ChunkDB
symbMap = cdb ([] :: [QuantityDict]) ([] :: [IdeaDict])
  ([] :: [ConceptChunk]) ([] :: [UnitDefn]) [] [] [] [] [] [] [] allRefs

usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) ([] :: [IdeaDict])
           ([] :: [ConceptChunk]) ([] :: [UnitDefn]) [] [] [] [] [] [] [] ([] :: [Reference])

allRefs :: [Reference]
allRefs = map rw sections ++ [imageRef, gitHubRef, caseStudyTabRef, docsRef, fullDocsRef, dataTableHTMLRef, dataTableCSVRef] 
  ++ concatMap snd (concat exampleDoxRefs) ++ concatMap snd (concat exampleCodeRefs) ++ drasilDepGraphRefs
  ++ map getHTMLRef exampleTitles ++ map getPDFRef exampleTitles

-- need section references
headerSecRef, introSecRef, caseStudySecRef, exampleSecRef, docsSecRef, analysisSecRef, graphSecRef :: Reference
headerSecRef = makeSecRef "Header" $ S "Header"
introSecRef = makeSecRef "Introduction" $ S "Introduction"
caseStudySecRef = makeSecRef "Case Study" $ S "Case Study"
exampleSecRef = makeSecRef "Examples" $ S "Examples"
docsSecRef = makeSecRef "Documentation" $ S "Documentation"
analysisSecRef = makeSecRef "Analysis" $ S "Analysis"
graphSecRef = makeSecRef "Dependency Graphs" $ S "Dependency Graphs"

--Hardcoded version of existing website.--

--hack for si to work
webName, web :: CI
webName = commonIdea "websiteName" (cn websiteTitle) "Drasil" []
web = commonIdea "website" (cn "website") "web" []

--header
headerSec :: Section
headerSec = section EmptyS [mkParagraph (S gitHubInfoName +:+ makeRef2S gitHubRef), imageContent] [] headerSecRef

imageContent :: Contents
imageContent = mkFig imageRef $ fig EmptyS imagePath

imageRef :: Reference
imageRef = makeFigRef "Drasil"

gitHubRef :: Reference
gitHubRef = Reference "gitHubRepo" (URI gitHubInfoURL) (shortname' $ S "gitHubRepo") None

websiteTitle, gitHubInfoName :: String
gitHubInfoURL, imagePath :: FilePath
websiteTitle = "Drasil - Generate All the Things!"
gitHubInfoName = "Link to GitHub repository:"
gitHubInfoURL = "https://github.com/JacquesCarette/Drasil"
imagePath = "./website/images/Icon.png"

-- intro
introSec :: Section
introSec = section EmptyS (map (mkParagraph.S) [introParagraph1, introParagraph2]) [] introSecRef

introParagraph1, introParagraph2 :: String
introParagraph1 = "Drasil is a framework for generating all of the software artifacts from a stable knowledge base, \
  \focusing currently on scientific software. The main goals are to reduce knowledge duplication and \
  \improve traceability. The atifacts are generated from a common knowledge-base using recipes \
  \written in a Domain-Specific Language (DSL). These recipes allow us to specify which pieces of \
  \knowledge should be used in which artifacts, how to transform them, and more."
-- need sentence below?
introParagraph2 = "This webpage is designed to contain the most up to date " ++
  "case study artifacts, Haddock documentation, and package dependency graphs " --foldlList "," List [caseStudy, haddockDocs, packDepGraph] 
  ++ "from the Drasil repository. \
  \The case study artifacts include the Software Requirements Specification (SRS) for the case study, \
  \which specifies what the program sets out to achieve. \
  \The Haddock Documentation section contains the current documentation for the Drasil framework. \
  \The package dependency graphs shows the hierarchy of modules within each package. \
  \The footer of this page contains the continuous integration build of the project, \
  \as well as the commit number that the build and artifacts are based off of."

----------------------------------------------------
-- case studies section
caseStudySec :: Section
caseStudySec = section (S caseStudiesTitle) [mkParagraph $ S caseStudiesDesc, mkFig caseStudyTabRef mkCaseTable, UlC $ ulcc caseStudyLegend] [] caseStudySecRef

caseStudiesTitle, caseStudiesDesc :: String
caseStudiesTitle = "Case Studies"
caseStudiesDesc = "Drasil allows some design decisions to be made by the user when generating \
  \code. The table below summarizes the design decisions made for each case \
  \study, followed by a guide giving the meaning of the short-forms used in the \
  \table:"
-- case studies table
mkCaseTable :: RawContent
mkCaseTable = Table headerRow tableBody EmptyS False

headerRow :: [Sentence]
headerRow = map S ["Case Study", modularityTitle, implementTypeTitle, loggingTitle, inStructTitle, conStructTitle, conRepTitle, realNumRepTitle]

tableBody :: [[Sentence]]
tableBody = mkTable [(\(CS x _ _ _ _ _ _ _) -> S x),
                     (\(CS _ x _ _ _ _ _ _) -> S $ show x),
                     (\(CS _ _ x _ _ _ _ _) -> S $ show x),
                     (\(CS _ _ _ x _ _ _ _) -> S $ show x),
                     (\(CS _ _ _ _ x _ _ _) -> S $ show x),
                     (\(CS _ _ _ _ _ x _ _) -> S $ show x),
                     (\(CS _ _ _ _ _ _ x _) -> S $ show x),
                     (\(CS _ _ _ _ _ _ _ x) -> S $ show x)]
                     [glassBRCase, noPCMCase, pdControllerCase, projectileCase1, projectileCase2, projectileCase3, projectileCase4, projectileCase5]

caseStudyTabRef :: Reference
caseStudyTabRef = makeTabRef "CaseStudy"

type Name = String
data Modularity = CMod | SMod | UMod
data ImplementType = LIm | PIm
data Logging = L | NoL deriving (Show)
data InStruct = BIn | UIn
data ConStruct = B | I | U | WI deriving (Show)
data ConRep = C | V deriving (Show)
data RealNumRep = D | F deriving (Show)

instance Show Modularity where
  show CMod = "C"
  show SMod = "S"
  show UMod = "U"

instance Show ImplementType where
  show LIm = "L"
  show PIm = "P"

instance Show InStruct where
  show BIn = "B"
  show UIn = "U"
data CaseStudy = CS Name Modularity ImplementType Logging InStruct ConStruct ConRep RealNumRep

glassBRCase, noPCMCase, pdControllerCase, projectileCase1, projectileCase2,
  projectileCase3, projectileCase4, projectileCase5 :: CaseStudy

glassBRCase      = CS "GlassBR"                     SMod PIm L   BIn I  C D
noPCMCase        = CS "NoPCM"                       CMod PIm NoL UIn B  C D
pdControllerCase = CS "PDController"                CMod PIm NoL UIn B  C D
projectileCase1  = CS "Projectile_C_P_NoL_B_U_V_D"  CMod PIm NoL BIn U  V D
projectileCase2  = CS "Projectile_S_L_NoL_U_U_V_F"  SMod LIm NoL UIn U  V F
projectileCase3  = CS "Projectile_U_P_L_B_B_C_D"    UMod PIm L   BIn B  C D
projectileCase4  = CS "Projectile_U_P_NoL_U_WI_V_D" UMod PIm NoL UIn WI V D
projectileCase5  = CS "Projectile_U_P_L_B_WI_V_F"   UMod PIm L   BIn WI V F

-- case studies symbol legend
modularityTitle, implementTypeTitle, loggingTitle, inStructTitle, conStructTitle, 
  conRepTitle, realNumRepTitle :: String
modPt1, modPt2, modPt3, implementPt1, implementPt2, 
  logPt1, logPt2, inStructPt1, inStructPt2, conStructPt1, conStructPt2, conStructPt3, 
  conStructPt4, conRepPt1, conRepPt2, realNumRepPt1, realNumRepPt2 :: String
modSymbPt1, modSymbPt2, modSymbPt3, implementSymbPt1, implementSymbPt2, 
  logSymbPt1, logSymbPt2, inStructSymbPt1, inStructSymbPt2, conStructSymbPt1, conStructSymbPt2, conStructSymbPt3, 
  conStructSymbPt4, conRepSymbPt1, conRepSymbPt2, realNumRepSymbPt1, realNumRepSymbPt2 :: String

caseStudyLegend :: RawContent
caseStudyLegend = Enumeration caseStudyList

caseStudyList :: ListType
caseStudyList = Bullet $ zip (zipWith3 mkLegendListFunc legendTitles legendSymbs legendConts) $ repeat Nothing

mkLegendListFunc :: Sentence -> [Sentence] -> [Sentence] -> ItemType
mkLegendListFunc t symbs conts = Nested t $ Bullet $ zip (zipWith mkTandDSent symbs conts) $ repeat Nothing

mkTandDSent :: Sentence -> Sentence -> ItemType
mkTandDSent s def = Flat $ s +:+ S "-" +:+ def

legendTitles :: [Sentence]
legendTitles = map S [modularityTitle, implementTypeTitle, loggingTitle, inStructTitle, conStructTitle, conRepTitle, realNumRepTitle]

legendSymbs :: [[Sentence]]
legendSymbs = map (map S) [[modSymbPt1, modSymbPt2, modSymbPt3],
                           [implementSymbPt1, implementSymbPt2],
                           [logSymbPt1, logSymbPt2],
                           [inStructSymbPt1, inStructSymbPt2],
                           [conStructSymbPt1, conStructSymbPt2, conStructSymbPt3, conStructSymbPt4],
                           [conRepSymbPt1, conRepSymbPt2],
                           [realNumRepSymbPt1, realNumRepSymbPt2]]

legendConts :: [[Sentence]]
legendConts = map (map S) [[modPt1, modPt2, modPt3],
                           [implementPt1, implementPt2],
                           [logPt1, logPt2],
                           [inStructPt1, inStructPt2],
                           [conStructPt1, conStructPt2, conStructPt3, conStructPt4],
                           [conRepPt1, conRepPt2],
                           [realNumRepPt1, realNumRepPt2]]

modularityTitle = "Modularity"
implementTypeTitle = "Implementation Type"
loggingTitle = "Logging"
inStructTitle = "Input Structure"
conStructTitle = "Constant Structure"
conRepTitle = "Constant Representation"
realNumRepTitle = "Real Number Representation"

modPt1 = "Unmodular"
modPt2 = "Modular with Combined input module"
modPt3 = "Modular with Separated input module"
modSymbPt1 = "U"
modSymbPt2 = "C"
modSymbPt3 = "S"

implementPt1 = "Program"
implementPt2 = "Library"
implementSymbPt1 = "P"
implementSymbPt2 = "L"

logPt1 = "No Logging statements"
logPt2 = "Logging statements included"
logSymbPt1 = "NoL"
logSymbPt2 = "L"

inStructPt1 = "Inputs are Bundled in a class"
inStructPt2 = "Inputs are Unbundled"
inStructSymbPt1 = "B"
inStructSymbPt2 = "U"

conStructPt1 = "Constant values are Inlined"
conStructPt2 = "Constants are stored With the Inputs"
conStructPt3 = "Constants are stored in variables that are Bundled in a class"
conStructPt4 = "Constants are stored in variables that are Unbundled"
conStructSymbPt1 = "I"
conStructSymbPt2 = "WI"
conStructSymbPt3 = "B"
conStructSymbPt4 = "U"

conRepPt1 = "Constants are stored as Variables"
conRepPt2 = "Constants are stored as Constants"
conRepSymbPt1 = "V"
conRepSymbPt2 = "C"

realNumRepPt1 = "Real numbers are represented as Doubles"
realNumRepPt2 = "Real numbers are represented as Floats"
realNumRepSymbPt1 = "D"
realNumRepSymbPt2 = "F"

----------------------------------------
-- example section
exampleSec :: Section
exampleSec = section (S exampleTitle) [mkParagraph exampleIntro, UlC $ ulcc mkExampleList] [] exampleSecRef

exampleTitle :: String
exampleTitle = "Generated Examples"

exampleIntro :: Sentence
exampleIntro = S "Each of the case studies contain their own generated PDF and HTML reports," +:+
  S "and in some cases, their own generated code."

mkExampleList :: RawContent
mkExampleList = Enumeration exampleList

exampleList :: ListType
exampleList = Bullet $ zip (zipWith4 mkExampleListFunc exampleTitles exampleDescs exampleCodeRefs exampleDoxRefs) $ repeat Nothing

mkExampleListFunc :: String -> String -> [(String, [Reference])] -> [(String, [Reference])] -> ItemType
mkExampleListFunc exmpl desc codePth doxPth
  | map snd codePth == [[]] && map snd doxPth == [[]] = Nested (S exmpl +:+ S desc) $ Bullet [(Flat (S (exmpl ++ "SRS") +:+ makeRef2S (getHTMLRef exmpl) +:+ makeRef2S (getPDFRef exmpl)), Nothing)]
  | map snd doxPth == [[]]                         = Nested (S exmpl +:+ S desc) $ Bullet $ zip [Flat $ S (exmpl ++ "SRS") +:+ makeRef2S (getHTMLRef exmpl) +:+ makeRef2S (getPDFRef exmpl),
                                                                       Nested (S generatedCodeTitle) $ Bullet $ mkCodeList codePth] $ repeat Nothing
                                                                              {-(Bullet [(foldlSent_ (map makeRef2S codePth), Nothing)]), Nothing)])-}
  {-| length codePth <= 1                        = Nested (S exmpl) (Bullet [((S (exmpl ++ "SRS")) +:+ makeRef2S getHTMLRef +:+ makeRef2S getPDFRef, Nothing),
                                                                       (Nested (S generatedCodeTitle) 
                                                                              (Bullet [(foldlSent_ (map makeRef2S codePth), Nothing)]), Nothing),
                                                                       (Nested (S generatedCodeDocsTitle) 
                                                                              (Bullet [(foldlSent_ (map makeRef2S doxPth), Nothing)]), Nothing)])-}
  {-| otherwise                                  = Nested (S exmpl) (Bullet [(Flat ((S (exmpl ++ "SRS")) +:+ makeRef2S getHTMLRef +:+ makeRef2S getPDFRef), Nothing),
                                                                       (Nested (S generatedCodeTitle) (Bullet (mkCodeList codePth),
                                                                       (Nested (S generatedCodeDocsTitle) (Bullet (mkCodeList doxPth)])-}
  | otherwise                                  = Nested (S exmpl +:+ S desc) $ Bullet $ zip [Flat $ S (exmpl ++ "SRS") +:+ makeRef2S (getHTMLRef exmpl) +:+ makeRef2S (getPDFRef exmpl),
                                                                       Nested (S generatedCodeTitle) $ Bullet $ mkCodeList codePth,
                                                                       Nested (S generatedCodeDocsTitle) $ Bullet $ mkCodeList doxPth] $ repeat Nothing

mkCodeList :: [(String, [Reference])] -> [(ItemType, Maybe String)]
mkCodeList [] = []
mkCodeList (r:refs) = (Flat $ foldlSent_ (map makeRef2S (snd r)), Nothing): mkCodeList refs

exampleTitles, exampleDescs :: [String]
exampleCodeRefs, exampleDoxRefs :: [[(String, [Reference])]]
exampleTitles = [pendulum, gamePhys, glassBR, hghc, noPCM, pdController, projectile, ssp, swhs, template]
exampleDescs = [pendulumDesc, gamePhysDesc, glassBRDesc, hghcDesc, noPCMDesc, pdControllerDesc, projectileDesc, sspDesc, swhsDesc, templateDesc]
exampleCodeRefs =[[(pendulum, [])],
                  [(gamePhys, [])],
                  [(glassBR, map (getCodeRef currHash glassBR) glassBRCode)],
                  [(hghc, [])],
                  [(noPCM, map (getCodeRef currHash noPCM) noPCMCode)],
                  [(pdController, map (getCodeRef currHash pdController) pdControllerCode)],
                  [(projectileC1, map (getCodeRef currHash (projectile ++ "/" ++ projectileC1)) projectileCase1Code),
                  (projectileC2, map (getCodeRef currHash (projectile ++ "/" ++ projectileC2)) projectileCase2Code),
                  (projectileC3, map (getCodeRef currHash (projectile ++ "/" ++ projectileC3)) projectileCase3Code),
                  (projectileC4, map (getCodeRef currHash (projectile ++ "/" ++ projectileC4)) projectileCase4Code),
                  (projectileC5, map (getCodeRef currHash (projectile ++ "/" ++ projectileC5)) projectileCase5Code)],
                  [(ssp, [])],
                  [(swhs, [])],
                  [(template, [])]]
exampleDoxRefs =[[(pendulum, [])],
                 [(gamePhys, [])],
                 [(glassBR, map (getDoxRef glassBR) glassBRDox)], 
                 [(hghc, [])], 
                 [(noPCM, map (getDoxRef noPCM) noPCMDox)],
                 [(pdController, map (getDoxRef pdController) pdControllerDox)],
                 [(projectileC1, map (getDoxRef (projectile ++ "/" ++ projectileC1)) projectileCase1Dox),
                 (projectileC2, map (getDoxRef (projectile ++ "/" ++ projectileC2)) projectileCase2Dox),
                 (projectileC3, map (getDoxRef (projectile ++ "/" ++ projectileC3)) projectileCase3Dox),
                 (projectileC4, map (getDoxRef (projectile ++ "/" ++ projectileC4)) projectileCase4Dox),
                 (projectileC5, map (getDoxRef (projectile ++ "/" ++ projectileC5)) projectileCase5Dox)],
                 [(ssp, [])],
                 [(swhs, [])],
                 [(template, [])]]

--example names, maybe make a unique type to accept fields of documents, gen code, and doxygen?
pendulum, gamePhys, glassBR, hghc, noPCM, pdController, projectile, projectileC1,
  projectileC2, projectileC3, projectileC4, projectileC5, ssp, swhs, template :: String

pendulum = "DblPendulum"
gamePhys = "GamePhysics"
glassBR = "GlassBR"
hghc = "HGHC"
noPCM = "NoPCM"
pdController = "PDController"
projectile = "Projectile"
projectileC1 = "Projectile_C_P_NoL_B_U_V_D"
projectileC2 = "Projectile_S_L_NoL_U_U_V_F"
projectileC3 = "Projectile_U_P_L_B_B_C_D"
projectileC4 = "Projectile_U_P_NoL_U_WI_V_D"
projectileC5 = "Projectile_U_P_L_B_WI_V_F"
ssp = "SSP"
swhs = "SWHS"
template = "Template"

glassBRCode, glassBRDox, noPCMCode, noPCMDox, pdControllerCode, pdControllerDox, projectileCase1Code, projectileCase2Code,
  projectileCase3Code, projectileCase4Code, projectileCase5Code, projectileCase1Dox, projectileCase2Dox,
  projectileCase3Dox, projectileCase4Dox, projectileCase5Dox :: [String]

glassBRCode          = ["cpp", "csharp", "java", "python", "swift"]
glassBRDox           = ["cpp", "csharp", "java", "python"]
noPCMCode            = ["cpp", "csharp", "java", "python"]
noPCMDox             = ["cpp", "csharp", "java", "python"]
pdControllerCode     = ["python"]
pdControllerDox      = []
projectileCase1Code  = ["cpp", "csharp", "java", "python", "swift"]
projectileCase2Code  = ["cpp", "csharp", "java", "python", "swift"]
projectileCase3Code  = ["cpp", "csharp", "java", "python", "swift"]
projectileCase4Code  = ["cpp", "csharp", "java", "python", "swift"]
projectileCase5Code  = ["cpp", "csharp", "java", "python", "swift"]
projectileCase1Dox   = ["cpp", "csharp", "java", "python"]
projectileCase2Dox   = ["cpp", "csharp", "java", "python"]
projectileCase3Dox   = ["cpp", "csharp", "java", "python"]
projectileCase4Dox   = ["cpp", "csharp", "java", "python"]
projectileCase5Dox   = ["cpp", "csharp", "java", "python"]

getHTMLRef, getPDFRef :: String -> Reference
getHTMLRef ex = Reference ("htmlRef" ++ ex) (URI (getHTMLPath ex)) (shortname' $ S ("htmlRef" ++ ex)) None
getPDFRef ex = Reference ("pdfRef" ++ ex) (URI (getPDFPath ex)) (shortname' $ S ("pdfRef" ++ ex)) None
getHTMLPath, getPDFPath :: String -> FilePath
getHTMLPath ex = "https://jacquescarette.github.io/Drasil/examples/" ++ ex ++ "/srs/" ++ ex ++ "_SRS.html"
getPDFPath ex = "https://jacquescarette.github.io/Drasil/examples/" ++ ex ++ "/srs/" ++ ex ++ "_SRS.pdf"

getCodeRef :: String -> String -> String -> Reference
getDoxRef :: String -> String -> Reference
getCodeRef hash ex lang = Reference ("codeRef" ++ ex ++ lang) (URI (getCodePath hash ex lang)) (shortname' $ S ("codeRef" ++ ex ++ lang)) None
getDoxRef ex lang = Reference ("doxRef" ++ ex ++ lang) (URI (getDoxPath ex lang)) (shortname' $ S ("doxRef" ++ ex ++ lang)) None
getCodePath :: String -> String -> String -> FilePath
getDoxPath :: String -> String -> FilePath
getCodePath hash ex lang = "https://github.com/JacquesCarette/Drasil/tree/" ++ hash ++ "/code/stable/" ++ ex ++ "/src/" ++ lang
getDoxPath ex lang = "https://jacquescarette.github.io/Drasil/examples/" ++ ex ++ "/doxygen/" ++ lang ++ "/index.html"

-- manually get commit number for now
currHash :: FilePath
currHash = "cef7307aa11ac0dc0ec1773b82311ca1f7bb6a51"

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
--docs section

docsSec :: Section
docsSec = section haddockDocsTitle [mkParagraph haddockDocsDesc] [] docsSecRef

haddockDocsTitle, haddockDocsDesc :: Sentence
docsPath, fullDocsPath :: FilePath
docsRef, fullDocsRef :: Reference

haddockDocsTitle = S "Haddock Documentation"
haddockDocsDesc = S "The current Haddock documentation" +:+ makeRef2S docsRef +:+ S "for the Drasil framework. A variant with fully exposed modules" +:+ makeRef2S fullDocsRef +:+ S "is also available."
docsPath = "docs/index.html"
fullDocsPath = "docs/full/index.html"
docsRef = Reference "haddockDocs" (URI docsPath) (shortname' $ S "HaddockDocs") None
fullDocsRef = Reference "fullHaddockDocs" (URI fullDocsPath) (shortname' $ S "fullHaddockDocs") None

---------------------------------------------------------
--analysis section

analysisSec :: Section
analysisSec = section drasilDataTableTitle [mkParagraph dataTableDesc] [] analysisSecRef

drasilDataTableTitle, dataTableDesc :: Sentence
dataTableHTMLPath, dataTableCSVPath :: FilePath
dataTableHTMLRef, dataTableCSVRef :: Reference

drasilDataTableTitle = S "Drasil Data Table"
dataTableDesc = S "Here is the updated Data Table" +:+ makeRef2S dataTableHTMLRef +:+ S "for the Drasil framework. There is also a downloadable version" +:+ makeRef2S dataTableCSVRef +:+ S "(csv format)."
dataTableHTMLPath = "analysis/DataTable.html"
dataTableCSVPath = "analysis/DataTable.csv"
dataTableHTMLRef = Reference "dataTableHTML" (URI dataTableHTMLPath) (shortname' $ S "dataTableHTML") None
dataTableCSVRef = Reference "dataTableCSV" (URI dataTableCSVPath) (shortname' $ S "dataTableCSV") None

----------------------------------------------------------
--graphs section

graphSec :: Section
graphSec = section packDepGraphTitle [UlC $ ulcc folderList] [] graphSecRef

packDepGraphTitle :: Sentence
drasilFolders, drasilDepGraphPaths :: [String]
drasilDepGraphRefs :: [Reference]

packDepGraphTitle = S "Package Dependency Graphs"
drasilFolders = ["drasil-build", "drasil-code", "drasil-data", "drasil-database", "drasil-docLang", "drasil-example", "drasil-gen", "drasil-gool", "drasil-lang", "drasil-printers", "drasil-theory", "drasil-utils"]
drasilDepGraphPaths = map (\x -> "graphs/" ++ x ++ ".pdf") drasilFolders
drasilDepGraphRefs = zipWith (\x y -> Reference x (URI y) (shortname' $ S x) None) drasilFolders drasilDepGraphPaths

folderList :: RawContent
folderList = Enumeration $ Bullet $ zip folderList' $ repeat Nothing

folderList' :: [ItemType]
folderList' = map Flat (zipWith (\x y -> S x +:+ makeRef2S y) drasilFolders drasilDepGraphRefs)



