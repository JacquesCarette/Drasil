module Drasil.Website.Body where

import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)
import Database.Drasil (Block, ChunkDB, SystemInformation(SI), cdb,
  rdb, refdb, _authors, _concepts, _constants, _constraints, _purpose,
  _datadefs, _instModels, _configFiles, _defSequence, _inputs, _kind, _outputs, _quants, 
  _sys, _sysinfodb, _usedinfodb, _folderPath)
import Language.Drasil
import Drasil.DocLang (findAllRefs)

import Drasil.Website.Introduction
import Drasil.Website.CaseStudy
import Drasil.Website.Example
import Drasil.Website.Documentation
import Drasil.Website.Analysis
import Drasil.Website.Graphs

-- Printing info to get document to generate. Takes in the 'FolderLocation'.
printSetting :: FolderLocation -> PrintingInformation
printSetting fl = PI (symbMap fl) Equational defaultConfiguration

-- Instead of being an SRSDecl, this takes the folder locations and generates the document from there.
mkWebsite :: FolderLocation -> Document
mkWebsite fl =
    --Document  -- Title  --  author  (hack for now to show up in proper spot) -- no table of contents -- [Section]
    Document (S websiteTitle) (namedRef gitHubRef (S "Link to GitHub Repository")) NoToC $ sections fl

-- Folder locations based on environment variables (using getEnv on Main.hs)
data FolderLocation = Folder {
        depL :: FilePath
    ,   docsRt :: FilePath
    ,   exRt :: FilePath
    --,   srsD :: FilePath
    --,   doxD :: FilePath
    ,   graphRt :: FilePath
    ,   analysisRt :: FilePath
    ,   repoRt :: FilePath
    ,   buildNum :: FilePath
    ,   buildPth :: FilePath
    ,   packages :: [String]
    ,   typeGraphFolder :: FilePath
    ,   classInstFolder :: FilePath
    }

-- System information. This probably isn't needed right now since it is really only used in SRS declarations.
si :: FolderLocation -> SystemInformation
si fl = SI {
    _sys         = webName,
    _kind        = web,
    _authors     = [] :: [Person],
    _quants      = [] :: [QuantityDict],
    _purpose     = [],
    _concepts    = [] :: [UnitaryConceptDict],
    _instModels  = [],
    _datadefs    = [],
    _configFiles = [],
    _folderPath  = "",
    _inputs      = [] :: [QuantityDict],
    _outputs     = [] :: [QuantityDict],
    _defSequence = [] :: [Block QDefinition],
    _constraints = [] :: [ConstrainedChunk],
    _constants   = [] :: [QDefinition],
    _sysinfodb   = symbMap fl,
    _usedinfodb  = usedDB,
    refdb        = rdb [] []
}

-- Puts all the sections in order. Basically the website version of the SRS declaration.
sections :: FolderLocation -> [Section]
-- Section Title [SecCons] Reference
sections fl = [headerSec, introSec (ref caseStudySec) (ref $ docsSec $ docsRt fl) (ref $ graphSec (graphRt fl) $ packages fl), 
  caseStudySec, exampleSec (repoRt fl) (exRt fl), docsSec (docsRt fl), analysisSec (analysisRt fl), graphSec (graphRt fl) $ packages fl,
  footer fl]

-- symbMap needed for references to work
symbMap :: FolderLocation -> ChunkDB
symbMap fl = cdb ([] :: [QuantityDict]) (map nw [webName, web] ++ map getSysName allExampleSI)
  ([] :: [ConceptChunk]) ([] :: [UnitDefn]) [] [] [] [] [] [] [] $ allRefs fl

getSysName :: SystemInformation -> IdeaDict
getSysName SI{_sys = nm} = nw nm 

-- needed for si to work
usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) ([] :: [IdeaDict])
           ([] :: [ConceptChunk]) ([] :: [UnitDefn]) [] [] [] [] [] [] [] ([] :: [Reference])

-- Holds all references and links used in the website.
allRefs :: FolderLocation -> [Reference]
allRefs fl = [headerSecRef, imageRef, gitHubRef] ++ map ref (sections fl) 
  ++ introRefs (ref caseStudySec) (ref $ docsSec $ docsRt fl) (ref $ graphSec (graphRt fl) $ packages fl) 
  ++ caseStudyRefs ++ exampleRefs (repoRt fl) (exRt fl) ++ docRefs (docsRt fl) ++ analysisRefs (analysisRt fl) ++ graphRefs (graphRt fl) (packages fl)
  ++ concatMap findAllRefs (sections fl)

-- Each section needs its own reference. Start with the header section.
headerSecRef :: Reference
headerSecRef = makeSecRef "Header" $ S "Header"

--hack for si to work. Probably unneeded.
webName, web :: CI
webName = commonIdea "websiteName" (cn websiteTitle) "Drasil" []
web = commonIdea "website" (cn "website") "web" []

-------------------------------
-- Header Section
-------------------------------

headerSec :: Section
headerSec = section EmptyS [LlC imageContent] [] headerSecRef

-- For the drasil tree image on the website
imageContent :: LabelledContent
imageContent = llcc imageRef $ figWithWidth EmptyS imagePath 50

-- LabelledContent needs a reference. Treats the image like an SRS Figure.
imageRef :: Reference
imageRef = makeFigRef "Drasil"

-- Used for the repository link.
gitHubRef :: Reference
gitHubRef = Reference "gitHubRepo" (URI gitHubInfoURL) (shortname' $ S "gitHubRepo")

-- Hardcoded info for the title, URL, and image path.
websiteTitle :: String
gitHubInfoURL, imagePath :: FilePath
websiteTitle = "Drasil - Generate All the Things!"
gitHubInfoURL = "https://github.com/JacquesCarette/Drasil"
imagePath = "./images/Icon.png"

---------------------
-- Footer Section
---------------------

footer :: FolderLocation -> Section
footer _ = section EmptyS [mkParagraph copyrightInfo] [] $ makeSecRef "Footer" $ S "Footer"

copyrightInfo :: Sentence
copyrightInfo = S "Copyright (c) Jacques Carette, 2021. All rights reserved. This website is a software artifact generated by Drasil."

-- uncomment to add in build number and path information
--buildInfo :: String -> String -> Sentence
--buildInfo bnum bPath = S $ "Build number: " ++ bnum ++ ". Generated from " ++ bPath ++ "."