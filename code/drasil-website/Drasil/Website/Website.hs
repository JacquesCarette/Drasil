module Drasil.Website.Website where

import Data.List (zipWith4)
import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)
import Database.Drasil (Block, ChunkDB, SystemInformation(SI), cdb,
  rdb, refdb, _authors, _concepts, _constants, _constraints, _purpose,
  _datadefs, _instModels, _configFiles, _defSequence, _inputs, _kind, _outputs, _quants, 
  _sys, _sysinfodb, _usedinfodb)
import Language.Drasil hiding (C)
import Utils.Drasil
import Data.Char (toUpper)

import Drasil.Website.Introduction
import Drasil.Website.CaseStudy (caseStudyRefs, caseStudySec)
import Drasil.Website.Example
import Drasil.Website.Documentation
import Drasil.Website.Analysis
import Drasil.Website.Graphs


printSetting :: PrintingInformation
printSetting = PI symbMap Equational defaultConfiguration

--mkWebsite :: SRSDecl -> SystemInformation -> Document
mkWebsite :: FolderLocation -> Document
mkWebsite fl =
    --Document   Title                         author  (hack for now)             [Section]
    Document (S websiteTitle) (namedRef gitHubRef (S "Link to GitHub Repository")) $ sections fl

data FolderLocation = Folder {
        depL :: FilePath
    ,   docsRt :: FilePath
    ,   exRt :: FilePath
    ,   srsD :: FilePath
    ,   doxD :: FilePath
    ,   graphRt :: FilePath
    ,   analysisRt :: FilePath
    ,   repoS :: FilePath
    ,   commitNum :: FilePath
    ,   buildNum :: FilePath
    ,   buildPth :: FilePath
    }

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

sections :: FolderLocation -> [Section]
-- Section Title [SecCons] Reference
sections fl = [headerSec, introSec (ref caseStudySec) (ref $ docsSec $ docsRt fl) (ref $ graphSec $ graphRt fl), 
  caseStudySec, exampleSec (repoS fl) (exRt fl), docsSec (docsRt fl), analysisSec (analysisRt fl), graphSec $ graphRt fl]

symbMap :: ChunkDB
symbMap = cdb ([] :: [QuantityDict]) ([] :: [IdeaDict])
  ([] :: [ConceptChunk]) ([] :: [UnitDefn]) [] [] [] [] [] [] [] allRefs

usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) ([] :: [IdeaDict])
           ([] :: [ConceptChunk]) ([] :: [UnitDefn]) [] [] [] [] [] [] [] ([] :: [Reference])

allRefs :: FolderLocation -> [Reference]
allRefs fl = [headerSecRef, imageRef, gitHubRef] ++ map ref (sections fl) 
  ++ introRefs (ref caseStudySec) (ref $ docsSec $ docsRt fl) (ref $ graphSec $ graphRt fl) 
  ++ caseStudyRefs ++ exampleRefs (repoS fl) (exRt fl) ++ docRefs (docsRt fl) ++ analysisRefs (analysisRt fl) ++ graphRefs $ graphRt fl

-- need section references
headerSecRef :: Reference
headerSecRef = makeSecRef "Header" $ S "Header"

--Hardcoded version of existing website.--

--hack for si to work
webName, web :: CI
webName = commonIdea "websiteName" (cn websiteTitle) "Drasil" []
web = commonIdea "website" (cn "website") "web" []

--header
headerSec :: Section
headerSec = section EmptyS [LlC imageContent] [] headerSecRef

--imageContent :: Contents
--imageContent = mkFig imageRef $ fig EmptyS imagePath
imageContent :: LabelledContent
imageContent = llcc imageRef $ figWithWidth EmptyS imagePath 50

imageRef :: Reference
imageRef = makeFigRef "Drasil"

gitHubRef :: Reference
gitHubRef = Reference "gitHubRepo" (URI gitHubInfoURL) (shortname' $ S "gitHubRepo") None

websiteTitle :: String
gitHubInfoURL, imagePath :: FilePath
websiteTitle = "Drasil - Generate All the Things!"
gitHubInfoURL = "https://github.com/JacquesCarette/Drasil"
imagePath = "../../drasil-website/images/Icon.png"




