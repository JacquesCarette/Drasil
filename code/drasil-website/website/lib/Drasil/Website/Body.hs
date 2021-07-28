-- | Gathers and organizes all the information for the [Drasil website](https://jacquescarette.github.io/Drasil/).
module Drasil.Website.Body where

import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)
import Database.Drasil (Block, ChunkDB, SystemInformation(SI), cdb,
  rdb, refdb, _authors, _concepts, _constants, _constraints, _purpose,
  _datadefs, _instModels, _configFiles, _defSequence, _inputs, _kind, _outputs, _quants, 
  _sys, _sysinfodb, _usedinfodb, _folderPath)
import Language.Drasil
import Drasil.DocLang (findAllRefs)

import Drasil.Website.Introduction (introSec, introRefs)
import Drasil.Website.CaseStudy (caseStudySec, caseStudyRefs)
import Drasil.Website.Example (exampleSec, exampleRefs, allExampleSI)
import Drasil.Website.Documentation (docsSec, docRefs)
import Drasil.Website.Analysis (analysisSec, analysisRefs)

-- * Functions to Generate the Website Through Drasil

-- | Printing info to get document to generate. Takes in the 'FolderLocation'.
printSetting :: FolderLocation -> PrintingInformation
printSetting fl = PI (symbMap fl) Equational defaultConfiguration

-- | Instead of being an 'SRSDecl', this takes the folder locations and generates the document from there.
mkWebsite :: FolderLocation -> Document
mkWebsite fl =
    --Document  -- Title  --  author  (hack for now to show up in proper spot) -- no table of contents -- [Section]
    Document (S websiteTitle) (namedRef gitHubRef (S "Link to GitHub Repository")) NoToC $ sections fl

-- | Folder locations based on environment variables (using 'getEnv' in "Drasil.Website.Main").
data FolderLocation = Folder {
    -- | Deploy location. Currently unused, but may be needed in the future.
    depL :: FilePath
    -- | Haddock documentation root file path. After using @make deploy@, this should be @deploy/docs@.
  , docsRt :: FilePath
    -- | Example root file path. After using @make deploy@, this should be @deploy/examples@.
  , exRt :: FilePath
    -- | Package dependency graph root file path. After using @make deploy@, this should be @deploy/graphs@.
  , graphRt :: FilePath
    -- | Analysis root file path. After using @make deploy@, this should be @deploy/analysis@.
  , analysisRt :: FilePath
    -- | Type graphs root file path. After using @make deploy@, this should be @deploy\/analysis\/TypeDependencyGraphs@.
  , typeGraphFolder :: FilePath
    -- | Class-instance graphs root file path. After using @make deploy@, this should be @deploy\/analysis\/DataTable\/packagegraphs@.
  , classInstFolder :: FilePath
    -- | Repository root, used for linking to generated code in GitHub.
  , repoRt :: FilePath
    -- | Deploy build number. Currently unused.
  , buildNum :: FilePath
    -- | Deploy build path. Currently unused.
  , buildPth :: FilePath
    -- | List of Drasil packages taken from the @Makefile@.
  , packages :: [String]
    }

-- | System information.
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

-- | Puts all the sections in order. Basically the website version of the SRS declaration.
sections :: FolderLocation -> [Section]
sections fl = [headerSec, introSec (ref caseStudySec) (ref $ docsSec $ docsRt fl) (ref $ analysisSec (analysisRt fl) (typeGraphFolder fl) (classInstFolder fl) (graphRt fl) $ packages fl) gitHubRef wikiRef, 
  exampleSec (repoRt fl) (exRt fl), caseStudySec, docsSec (docsRt fl), analysisSec (analysisRt fl) (typeGraphFolder fl) (classInstFolder fl) (graphRt fl) $ packages fl,
  footer fl]

-- | Needed for references and terms to work.
symbMap :: FolderLocation -> ChunkDB
symbMap fl = cdb ([] :: [QuantityDict]) (map nw [webName, web] ++ map getSysName allExampleSI)
  ([] :: [ConceptChunk]) ([] :: [UnitDefn]) [] [] [] [] [] [] [] $ allRefs fl

-- | Helper to get the system name as an 'IdeaDict' from 'SystemInformation'.
getSysName :: SystemInformation -> IdeaDict
getSysName SI{_sys = nm} = nw nm 

-- | Empty database needed for 'si' to work.
usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) ([] :: [IdeaDict])
           ([] :: [ConceptChunk]) ([] :: [UnitDefn]) [] [] [] [] [] [] [] ([] :: [Reference])

-- | Holds all references and links used in the website.
allRefs :: FolderLocation -> [Reference]
allRefs fl = [headerSecRef, imageRef, gitHubRef, wikiRef] ++ map ref (sections fl) 
  ++ introRefs (ref caseStudySec) (ref $ docsSec $ docsRt fl) (ref $ analysisSec (analysisRt fl) (typeGraphFolder fl) (classInstFolder fl) (graphRt fl) $ packages fl) gitHubRef wikiRef
  ++ caseStudyRefs ++ exampleRefs (repoRt fl) (exRt fl) ++ docRefs (docsRt fl) ++ analysisRefs (analysisRt fl) (typeGraphFolder fl) (classInstFolder fl) (graphRt fl) (packages fl)
  ++ concatMap findAllRefs (sections fl)

-- | Each section needs its own reference. Start with the header section.
headerSecRef :: Reference
headerSecRef = makeSecRef "Header" $ S "Header"

-- | Used for system name and kind inside of 'si'.
webName, web :: CI
webName = commonIdea "websiteName" (cn websiteTitle) "Drasil" []
web = commonIdea "website" (cn "website") "web" []

-- * Header Section

-- | Header section creator.
headerSec :: Section
headerSec = section EmptyS [LlC imageContent] [] headerSecRef

-- | For the drasil tree image on the website.
imageContent :: LabelledContent
imageContent = llcc imageRef $ figWithWidth EmptyS imagePath 50

-- | 'LabelledContent' needs a reference. Treats the image like an SRS Figure.
imageRef :: Reference
imageRef = makeFigRef "Drasil"

-- | Used for the repository link.
gitHubRef :: Reference
gitHubRef = Reference "gitHubRepo" (URI gitHubInfoURL) (shortname' $ S "gitHubRepo")
wikiRef :: Reference
wikiRef = Reference "gitHubWiki" (URI $ gitHubInfoURL ++ "/wiki") (shortname' $ S "gitHubWiki")

-- | Hardcoded info for the title, URL, and image path.
websiteTitle :: String
gitHubInfoURL, imagePath :: FilePath
websiteTitle = "Drasil - Generate All the Things!"
gitHubInfoURL = "https://github.com/JacquesCarette/Drasil"
imagePath = "./images/Icon.png"

-- * Footer Section

-- | Create the footer section.
footer :: FolderLocation -> Section
footer _ = section EmptyS [mkParagraph copyrightInfo] [] $ makeSecRef "Footer" $ S "Footer"

-- | 'footer' contents.
copyrightInfo :: Sentence
copyrightInfo = S "Copyright (c) Jacques Carette, 2021. All rights reserved. This website is a software artifact generated by Drasil."

-- uncomment to add in build number and path information
--buildInfo :: String -> String -> Sentence
--buildInfo bnum bPath = S $ "Build number: " ++ bnum ++ ". Generated from " ++ bPath ++ "."